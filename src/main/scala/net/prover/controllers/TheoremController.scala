package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.entries.Theorem
import net.prover.model.expressions.Statement
import net.prover.model.proof._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Failure, Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}"))
class TheoremController @Autowired() (val bookService: BookService) extends BookModification {
  case class InferenceSuggestion(
    inference: Inference.Summary,
    requiredSubstitutions: Substitutions.Required,
    substitutions: Seq[Substitutions])
  @GetMapping(value = Array("/{stepPath}/suggestInferences"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferences(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    (for {
      (book, chapter, theorem, step, stepContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, stepPath)
    } yield {
      implicit val parsingContext: ParsingContext = getStepParsingContext(book, chapter, theorem, stepContext)
      parsingContext.inferences
        .filter(_.name.toLowerCase.contains(searchText.toLowerCase))
        .reverse
        .mapCollect { inference =>
          val substitutions = inference.conclusion.calculateSubstitutions(step.statement, Substitutions.empty, 0, stepContext.externalDepth)
          if (substitutions.nonEmpty)
            Some(InferenceSuggestion(inference.summary, inference.requiredSubstitutions, substitutions))
          else
            None
        }
        .take(10)
    }).toResponseEntity
  }
  @GetMapping(value = Array("/{stepPath}/suggestPremises"), produces = Array("application/json;charset=UTF-8"))
  def suggestPremisesForInference(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("inferenceId") inferenceId: String
  ): ResponseEntity[_] = {
    (for {
      (book, chapter, theorem, step, stepContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, stepPath)
      parsingContext = getStepParsingContext(book, chapter, theorem, stepContext)
      inference <- findInference(inferenceId)(parsingContext)
    } yield {
      getPremiseMatches(inference.premises, inference.conclusion, step, stepContext, parsingContext)
    }).toResponseEntity
  }

  @GetMapping(value = Array("/{stepPath}/suggestNamingInferences"), produces = Array("application/json;charset=UTF-8"))
  def suggestNamingInferences(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    (for {
      (book, chapter, theorem, step, stepContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, stepPath)
    } yield {
      implicit val parsingContext: ParsingContext = getStepParsingContext(book, chapter, theorem, stepContext)
      ProofHelper.findNamingInferences(parsingContext)
        .filter(_._1.name.toLowerCase.contains(searchText.toLowerCase))
        .reverse
        .mapCollect { case (inference, namingPremises, _) =>
          val substitutions = inference.conclusion.calculateSubstitutions(step.statement, Substitutions.empty, 0, stepContext.externalDepth)
          if (substitutions.nonEmpty)
            Some(InferenceSuggestion(inference.summary.copy(premises = namingPremises), inference.requiredSubstitutions, substitutions))
          else
            None
        }
        .take(10)
    }).toResponseEntity
  }

  @GetMapping(value = Array("/{stepPath}/suggestNamingPremises"), produces = Array("application/json;charset=UTF-8"))
  def suggestPremisesForNaming(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("inferenceId") inferenceId: String
  ): ResponseEntity[_] = {
    (for {
      (book, chapter, theorem, step, stepContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, stepPath)
      parsingContext = getStepParsingContext(book, chapter, theorem, stepContext)
      inference <- findInference(inferenceId)(parsingContext)
      (namingPremises, _) <- ProofHelper.getNamingPremisesAndAssumption(inference, parsingContext).orBadRequest(s"Inference $inferenceId was not naming inference")
    } yield {
      getPremiseMatches(namingPremises, inference.conclusion, step, stepContext, parsingContext)
    }).toResponseEntity
  }


  case class PossiblePremiseMatch(statement: Statement, substitutions: Seq[Substitutions])
  private def getPremiseMatches(premises: Seq[Statement], conclusion: Statement, step: Step.Target, stepContext: StepContext, parsingContext: ParsingContext): Seq[Seq[PossiblePremiseMatch]] = {
    val possibleConclusionSubstitutions = conclusion.calculateSubstitutions(step.statement, Substitutions.empty, 0, stepContext.externalDepth)
    val availablePremises = ProofHelper.getAvailablePremises(stepContext, parsingContext).map(_.statement).distinct
    premises.map { premise =>
      availablePremises.mapCollect { availablePremise =>
        val substitutions = for {
          conclusionSubstitutions <- possibleConclusionSubstitutions
          premiseSubstitutions <- premise.calculateSubstitutions(availablePremise, conclusionSubstitutions, 0, stepContext.externalDepth)
        } yield premiseSubstitutions
        if (substitutions.nonEmpty) {
          Some(PossiblePremiseMatch(availablePremise, substitutions))
        } else {
          None
        }
      }
    }
  }

  @PostMapping(value = Array("/{stepPath}/clear"))
  def clearStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    replaceStep[Step](bookKey, chapterKey, theoremKey, stepPath)((step, _, _) =>
      Success(step.provenStatement.map(s => Step.Target(s)).toSeq)
    ).toResponseEntity
  }

  @DeleteMapping(value = Array("/{stepPath}"))
  def deleteStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    replaceStep[Step](bookKey, chapterKey, theoremKey, stepPath)((step, _, _) =>
      Success(Nil)
    ).toResponseEntity
  }

  @PostMapping(value = Array("/{stepPath}/move"))
  def moveStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @RequestParam("direction") direction: String
  ): ResponseEntity[_] = {
    def moveInTheorem(theorem: Theorem, parsingContext: ParsingContext): Option[Try[Theorem]] = {
      stepPath.indexes match {
        case Nil =>
          None
        case Seq(0) if direction == "up" =>
          Some(Failure(BadRequestException("Cannot move step upwards if it's the first step")))
        case init :+ containerIndex :+ 0 if direction == "up" =>
          theorem.tryModifySteps(init, (steps, _) => {
            steps.lift(containerIndex).flatMap { outerStep =>
              outerStep.extractSubstep(0).map(_.orBadRequest(s"Could not extract step $stepPath from outer step"))
            }.mapMap { case (outerStep, innerStep) =>
              steps.take(containerIndex) ++ Seq(innerStep, outerStep) ++ steps.drop(containerIndex + 1)
            }
          })
        case init :+ last =>
          theorem.tryModifySteps(init, (steps, _) => {
            steps.lift(last).map { step =>
              direction match {
                case "up" =>
                  Success((steps.take(last - 1) :+ step :+ steps(last - 1)) ++ steps.drop(last + 1))
                case "down" =>
                  Success((steps.take(last) ++ steps.lift(last + 1).toSeq :+ step) ++ steps.drop(last + 2))
                case _ =>
                  Failure(BadRequestException(s"Unrecognised direction $direction"))
              }
            }
          })
      }
    }
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, parsingContext) =>
      moveInTheorem(theorem, parsingContext).orNotFound(s"Step $stepPath").flatten
    }.toResponseEntity
  }


}
