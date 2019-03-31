package net.prover.controllers

import net.prover.controllers.models.StepDefinition
import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.entries.Theorem
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step.NewAssert
import net.prover.model.proof._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.core.convert.converter.Converter
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Component
import org.springframework.web.bind.annotation._

import scala.reflect._
import scala.util.{Failure, Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}"))
class TheoremController @Autowired() (bookService: BookService) {
  case class InferenceSuggestion(
    inference: Inference.Summary,
    requiredSubstitutions: Substitutions.Required,
    substitutions: Seq[Substitutions])
  @GetMapping(value = Array("/{stepReference}/suggestInferences"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferences(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    (for {
      (book, chapter, theorem, step, stepContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference)
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

  case class PossiblePremiseMatch(statement: Statement, substitutions: Seq[Substitutions])
  @GetMapping(value = Array("/{stepReference}/suggestPremises"), produces = Array("application/json;charset=UTF-8"))
  def suggestPremisesForSubstitutions(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData,
    @RequestParam("inferenceId") inferenceId: String
  ): ResponseEntity[_] = {
    (for {
      (book, chapter, theorem, step, stepContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference)
      parsingContext = getStepParsingContext(book, chapter, theorem, stepContext)
      inference <- findInference(inferenceId)(parsingContext)
    } yield {
      val possibleConclusionSubstitutions = inference.conclusion.calculateSubstitutions(step.statement, Substitutions.empty, 0, stepContext.externalDepth)
      val availablePremises = ProofHelper.getAvailablePremises(stepContext, parsingContext).map(_.statement).distinct
      inference.premises.map { premise =>
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
    }).toResponseEntity
  }

  @PutMapping(value = Array("/{stepReference}"))
  def createStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    replaceStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference) { (step, stepContext, parsingContext) =>
      for {
        inference <- findInference(definition.inferenceId)(parsingContext)
        substitutions <- definition.parseSubstitutions(inference)(parsingContext)
        premiseStatements <- Try(inference.substitutePremisesAndValidateConclusion(substitutions, step.statement, stepContext.externalDepth)).recoverWith { case e => Failure(BadRequestException(e.getMessage))}
      } yield {
        val premises = premiseStatements.map(createPremise(_, stepContext, parsingContext))
        val targetSteps = premises.ofType[Step.NewAssert.Premise.Pending].map(p => Step.Target(p.statement))
        targetSteps :+ Step.NewAssert(
          step.statement,
          inference,
          premiseStatements.map(createPremise(_, stepContext, parsingContext)),
          substitutions)
      }
    }.toResponseEntity
  }

  @DeleteMapping(value = Array("/{stepReference}"))
  def deleteStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData
  ): ResponseEntity[_] = {
    def getReplacementStep(step: Step): Try[Option[Step]] = {
      step match {
        case _: Step.Target =>
          Success(None)
        case _ =>
          Success(step.provenStatement.map(s => Step.Target(s)))
      }
    }
    replaceStep[Step](bookKey, chapterKey, theoremKey, stepReference)((step, _, _) =>
      getReplacementStep(step).map(_.toSeq)
    ).toResponseEntity
  }

  @PostMapping(value = Array("/{stepReference}/move"))
  def moveStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData,
    @RequestParam("direction") direction: String
  ): ResponseEntity[_] = {
    def moveInTheorem(theorem: Theorem, parsingContext: ParsingContext): Option[Try[Theorem]] = {
      stepReference.indexes match {
        case Nil =>
          None
        case Seq(0) if direction == "up" =>
          Some(Failure(BadRequestException("Cannot move step upwards if it's the first step")))
        case init :+ containerIndex :+ 0 if direction == "up" =>
          theorem.tryModifySteps(init, (steps, _) => {
            steps.lift(containerIndex).flatMap { outerStep =>
              outerStep.extractSubstep(0).map(_.orBadRequest(s"Could not extract step $stepReference from outer step"))
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
                  Success((steps.take(last) ++ steps.lift(last + 1).toSeq :+ step) ++ steps.drop(last + 1))
                case _ =>
                  Failure(BadRequestException(s"Unrecognised direction $direction"))
              }
            }
          })
      }
    }
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, parsingContext) =>
      moveInTheorem(theorem, parsingContext).orNotFound(s"Step $stepReference").flatten
    }.toResponseEntity
  }

  @PostMapping(value = Array("/{stepReference}/introduceBoundVariable"))
  def introduceBoundVariable(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData,
    @RequestBody variableName: String
  ): ResponseEntity[_] = {
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference) { (step, _, parsingContext) =>
      for {
        (substatement, scopingStatementDefinition) <- parsingContext.matchScopingStatement(step.statement).orBadRequest("Target statement is not a scoped statement")
      } yield {
        Step.ScopedVariable(
          variableName,
          Seq(Step.Target(substatement)),
          scopingStatementDefinition)
      }
    }.toResponseEntity
  }

  @PostMapping(value = Array("/{stepReference}/introduceDeduction"))
  def introduceDeduction(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData
  ): ResponseEntity[_] = {
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference) { (step, _, parsingContext) =>
      for {
        (antecedent, consequent, deductionStatementDefinition) <- parsingContext.matchDeductionStatement(step.statement).orBadRequest("Target statement is not a deduction statement")
      } yield {
        Step.Deduction(
          antecedent,
          Seq(Step.Target(consequent)),
          deductionStatementDefinition)
      }
    }.toResponseEntity
  }

  @PostMapping(value = Array("/{stepReference}/elide"))
  def elide(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData
  ): ResponseEntity[_] = {
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference) { (step, _, _) =>
      Success(Step.Elided(Seq(step), None))
    }.toResponseEntity
  }

  @PostMapping(value = Array("/{stepReference}/highlightedInference"))
  def elide(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData,
    @RequestBody inferenceId: String
  ): ResponseEntity[_] = {
    modifyStep[Step.Elided](bookKey, chapterKey, theoremKey, stepReference) { (step, _, parsingContext) =>
      for {
        inference <- findInference(inferenceId)(parsingContext)
      } yield step.copy(highlightedInference = Some(inference))
    }.toResponseEntity
  }

  @PostMapping(value = Array("/{stepPath}/createTargets"))
  def createTargets(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData
  ): ResponseEntity[_] = {
    replaceStep[Step.NewAssert](bookKey, chapterKey, theoremKey, stepPath) { (step, _, _) =>
      val targetStatements = step.pendingPremises.values.map(_.statement).toSeq
      Success(targetStatements.map(Step.Target(_)) :+ step)
    }.toResponseEntity
  }

  @PutMapping(value = Array(
    "/{stepPath}/boundVariables/{boundVariableIndex}",
    "/{stepPath}/boundVariables/{statementPath}/{boundVariableIndex}"))
  def renameBoundVariableInStep(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @PathVariable(value = "statementPath", required = false) statementPath: PathData,
    @PathVariable("boundVariableIndex") boundVariableIndex: Int,
    @RequestBody boundVariableName: String
  ): ResponseEntity[_] = {
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, stepPath) { (step, _, _) =>
      step.statement.renameBoundVariable(boundVariableName, boundVariableIndex, Option(statementPath).map(_.indexes).getOrElse(Nil)).orNotFound(s"Bound variable $boundVariableIndex at $statementPath")
        .map(newStatement => step.copy(statement = newStatement))
    }.toResponseEntity
  }

  private def findTheorem(bookKey: String, chapterKey: String, theoremKey: String): Try[(Book, Chapter, Theorem)] = {
    for {
      book <- bookService.books.find(_.key.value == bookKey).orNotFound(s"Book $bookKey")
      chapter <- book.chapters.find(_.key.value == chapterKey).orNotFound(s"Chapter $chapterKey")
      theorem <- chapter.entries.ofType[Theorem].find(_.key.value == theoremKey).orNotFound(s"Theorem $theoremKey")
    } yield (book, chapter, theorem)
  }

  private def findStep[T <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, stepReference: PathData): Try[(Book, Chapter, Theorem, T, StepContext)] = {
    for {
      (book, chapter, theorem) <- findTheorem(bookKey, chapterKey, theoremKey)
      (rawStep, stepContext) <- theorem.findStep(stepReference.indexes).orNotFound(s"Step $stepReference")
      step <- rawStep.asOptionalInstanceOf[T].orBadRequest(s"Step was not ${classTag[T].runtimeClass.getName}")
    } yield {
      (book, chapter, theorem, step, stepContext)
    }
  }

  private def modifyTheorem(bookKey: String, chapterKey: String, theoremKey: String)(f: (Theorem, ParsingContext) => Try[Theorem]): Try[Theorem] = {
    bookService.modifyEntry[Theorem, Theorem](bookKey, chapterKey, theoremKey, (_, book, chapter, theorem) => {
      val parsingContext = getTheoremParsingContext(book, chapter, theorem)
      f(theorem, parsingContext).map(_.recalculateReferences(parsingContext))
    }).map(_._4)
  }

  private def modifyStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, stepReference: PathData)(f: (TStep, StepContext, ParsingContext) => Try[Step]): Try[Theorem] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, parsingContext) =>
      theorem.tryModifyStep(stepReference.indexes, (step, stepContext) => {
        for {
          typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
          newStep <- f(typedStep, stepContext, parsingContext.copy(parameterLists = stepContext.boundVariableLists.map(_.zipWithIndex)))
        } yield newStep
      }).orNotFound(s"Step $stepReference").flatten
    }
  }

  private def replaceStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, stepReference: PathData)(f: (TStep, StepContext, ParsingContext) => Try[Seq[Step]]): Try[Theorem] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (theorem, parsingContext) =>
        (stepReference.indexes match {
          case init :+ last =>
            theorem.tryModifySteps(init, (steps, outerContext) => {
              steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
                for {
                  typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
                  replacementSteps <- f(typedStep, outerContext.addSteps(before), parsingContext.copy(parameterLists = outerContext.boundVariableLists.map(_.zipWithIndex)))
                } yield before ++ replacementSteps ++ after
              }
            })
          case _ =>
            None
        }).orNotFound(s"Step $stepReference").flatten
    }
  }

  private def getStepParsingContext(book: Book, chapter: Chapter, theorem: Theorem, stepContext: StepContext): ParsingContext = {
    getTheoremParsingContext(book, chapter, theorem).copy(parameterLists = stepContext.boundVariableLists.map(_.zipWithIndex))
  }

  private def findInference(inferenceId: String)(implicit parsingContext: ParsingContext): Try[Inference.Summary] = {
    parsingContext.inferences.find(_.id == inferenceId).map(_.summary).orBadRequest(s"Invalid inference $inferenceId")
  }

  private def createPremise(target: Statement, stepContext: StepContext, parsingContext: ParsingContext): NewAssert.Premise = {
    ProofHelper.findPremise(target, stepContext, parsingContext)
  }

  case class PathData(indexes: Seq[Int]) {
   override def toString: String = indexes.mkString(".")
  }
  @Component
  class StepReferenceConverter extends Converter[String, PathData] {
    override def convert(source: String): PathData = {
      PathData(source.split('.').map(_.toInt))
    }
  }
}
