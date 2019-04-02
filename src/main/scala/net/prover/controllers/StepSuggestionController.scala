package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model.expressions.Statement
import net.prover.model.{Inference, ParsingContext, Substitutions}
import net.prover.model.proof.{ProofHelper, Step, StepContext}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}/{stepPath}"))
class StepSuggestionController @Autowired() (val bookService: BookService) extends BookModification {

  case class InferenceSuggestion(
    inference: Inference.Summary,
    requiredSubstitutions: Substitutions.Required,
    substitutions: Seq[Substitutions])
  @GetMapping(value = Array("/suggestInferences"), produces = Array("application/json;charset=UTF-8"))
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

  @GetMapping(value = Array("/suggestPremises"), produces = Array("application/json;charset=UTF-8"))
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

  @GetMapping(value = Array("/suggestNamingInferences"), produces = Array("application/json;charset=UTF-8"))
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

  @GetMapping(value = Array("/suggestNamingPremises"), produces = Array("application/json;charset=UTF-8"))
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

}
