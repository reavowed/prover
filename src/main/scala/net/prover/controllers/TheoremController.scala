package net.prover.controllers

import net.prover.controllers.TheoremController._
import net.prover.controllers.models.StepDefinition
import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.entries.{StatementDefinition, TermDefinition, Theorem}
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step.NewAssert
import net.prover.model.proof._
import net.prover.services.BookService
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
  @GetMapping(value = Array("/{stepReference}/suggestInferences"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferences(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData,
    @RequestParam("searchText") searchText: String
  ): ResponseEntity[_] = {
    (for {
      (book, chapter, theorem, step) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference)
    } yield {
      implicit val parsingContext: ParsingContext = getStepParsingContext(book, chapter, theorem, step)
      parsingContext.inferences
        .filter(_.name.toLowerCase.contains(searchText.toLowerCase))
        .mapCollect { inference =>
          val substitutions = inference.conclusion.calculateSubstitutions(step.statement, Substitutions.empty, 0, step.context.externalDepth)
          if (substitutions.nonEmpty)
            Some(InferenceSuggestion(inference, inference.requiredSubstitutions, substitutions))
          else
            None
        }
        .reverse
        .take(10)
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
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference) { (book, chapter, theorem, step) =>
      implicit val stepContext: StepContext = step.context
      implicit val parsingContext: ParsingContext = getStepParsingContext(book, chapter, theorem, step)
      for {
        inference <- findInference(definition.inferenceId)
        substitutions <- definition.parseSubstitutions(inference, parsingContext)
        premiseStatements <- Try(inference.substitutePremisesAndValidateConclusion(substitutions, step.statement, stepContext.externalDepth)).recoverWith { case e => Failure(BadRequestException(e.getMessage))}
      } yield {
        Step.NewAssert(
          step.statement,
          inference,
          premiseStatements.map(createPremise(_, stepContext, parsingContext)),
          substitutions,
          step.context)
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
          Success(step.provenStatement.map(s => Step.Target(s, step.context)))
      }
    }
    def deleteFromTheorem(theorem: Theorem, parsingContext: ParsingContext): Option[Try[Theorem]] = {
      stepReference.indexes match {
        case Nil =>
          None
        case init :+ last =>
          theorem.tryModifySteps(init, steps => {
            steps.lift(last).map { step =>
              getReplacementStep(step).map {
                case Some(replacementStep) =>
                  steps.updated(last, replacementStep)
                case None =>
                  steps.take(last) ++ steps.drop(last + 1)
              }
            }
          })
          .mapMap(_.recalculateReferences(parsingContext))
      }
    }
    modifyTheorem(bookKey, chapterKey, theoremKey) { (book, chapter, theorem) =>
      val parsingContext = getTheoremParsingContext(book, chapter, theorem)
      deleteFromTheorem(theorem, parsingContext).orNotFound(s"Step $stepReference").flatten
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
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference) { (book, chapter, theorem, step) =>
      implicit val stepContext: StepContext = step.context
      implicit val parsingContext: ParsingContext = getStepParsingContext(book, chapter, theorem, step)
      for {
        (substatement, scopingStatementDefinition) <- parsingContext.matchScopingStatement(step.statement).orBadRequest("Target statement is not a scoped statement")
      } yield {
        Step.ScopedVariable(
          variableName,
          Seq(Step.Target(substatement, stepContext.addBoundVariable(variableName))),
          scopingStatementDefinition,
          stepContext)
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
    modifyStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference) { (book, chapter, theorem, step) =>
      implicit val stepContext: StepContext = step.context
      implicit val parsingContext: ParsingContext = getStepParsingContext(book, chapter, theorem, step)
      for {
        (antecedent, consequent, deductionStatementDefinition) <- parsingContext.matchDeductionStatement(step.statement).orBadRequest("Target statement is not a deduction statement")
      } yield {
        Step.Deduction(
          antecedent,
          Seq(Step.Target(consequent, stepContext)),
          deductionStatementDefinition,
          stepContext)
      }
    }.toResponseEntity
  }

  @PutMapping(value = Array(
    "/{stepReference}/premises/{premisePath}/statement/{expressionPath}/boundVariables/{boundVariableIndex}",
    "/{stepReference}/premises/{premisePath}/statement/boundVariables/{boundVariableIndex}"))
  def editBoundVariableName(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepPath: PathData,
    @PathVariable("premisePath") premisePath: PathData,
    @PathVariable(value = "expressionPath", required = false) expressionPath: PathData,
    @PathVariable("boundVariableIndex") boundVariableIndex: Int,
    @RequestBody newBoundVariableName: String
  ): ResponseEntity[_] = {
    import Step.NewAssert._
    def updatePremise(oldPremise: Premise): Try[Premise] = {
      for {
        oldPendingPremise <- oldPremise.asOptionalInstanceOf[Premise.Pending].orBadRequest(s"Premise $premisePath is not pending")
        newStatement <- oldPendingPremise.statement.renameBoundVariable(
          newBoundVariableName,
          boundVariableIndex,
          Option(expressionPath).map(_.indexes).getOrElse(Nil)
        ).orNotFound(s"Bound variable $boundVariableIndex at $expressionPath")
      } yield oldPendingPremise.copy(statement = newStatement)
    }
    modifyStep[Step.NewAssert](bookKey, chapterKey, theoremKey, stepPath) { (_, _, _, oldStep) =>
      oldStep.tryUpdatePremiseAtPath(premisePath.indexes, updatePremise).orNotFound(s"Premise $premisePath").flatten
    }.toResponseEntity
  }

  case class PremiseOption(path: Seq[Int], expansions: Seq[Inference.Summary])
  @GetMapping(value = Array("/{stepReference}/premiseOptions"))
  def premiseOptions(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData
  ): ResponseEntity[_] = {
    findStep[Step.NewAssert](bookKey, chapterKey, theoremKey, stepReference).map { case (book, chapter, theorem, step) =>
      val parsingContext = getStepParsingContext(book, chapter, theorem, step)
      val expansionInferences = parsingContext.inferences.filter(_.rearrangementType == Inference.RearrangementType.Expansion)
      step.pendingPremises.map { case (path, p) =>
        val matchingExpansions = expansionInferences.filter(_.conclusion.calculateSubstitutions(p.statement, Substitutions.empty, 0, step.context.externalDepth).nonEmpty)
        PremiseOption(path, matchingExpansions.map(_.summary))
      }
    }.toResponseEntity
  }

  @PostMapping(value = Array("/{stepPath}/premises/{premisePath}/target"))
  def createTarget(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @PathVariable("premisePath") premisePath: PathData
  ): ResponseEntity[_] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (book, chapter, theorem) =>
      for {
        rawStep <- theorem.findStep(stepPath.indexes).orNotFound(s"Step $stepPath")
        step <- rawStep.asOptionalInstanceOf[Step.NewAssert].orBadRequest(s"Step $stepPath is not editable")
        premise <- step.pendingPremises.get(premisePath.indexes).orNotFound(s"Premise $premisePath")
        newStep = Step.Target(premise.statement, step.context)
        newTheorem <- theorem.insertStep(stepPath.indexes, newStep).orBadRequest(s"Failed to insert new step")
          .map(_.recalculateReferences(getTheoremParsingContext(book, chapter, theorem)))
      } yield {
        newTheorem
      }
    }.toResponseEntity
  }

  @PostMapping(value = Array("/{stepPath}/premises/{premisePath}/rearrangement"))
  def createRearrangement(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepPath") stepPath: PathData,
    @PathVariable("premisePath") premisePath: PathData,
    @RequestBody inferenceId: String
  ): ResponseEntity[_] = {
    import Step.NewAssert._
    def updatePremise(oldPremise: Premise, stepContext: StepContext)(implicit parsingContext: ParsingContext): Try[Premise] = {
      for {
        inference <- findInference(inferenceId)
        substitutions <- inference.conclusion.calculateSubstitutions(oldPremise.statement, Substitutions.empty, 0, stepContext.externalDepth).headOption.orException(BadRequestException("Could not calculate substitutions"))
        substitutedInferencePremises <- Try(inference.substitutePremisesAndValidateConclusion(substitutions, oldPremise.statement, stepContext.externalDepth)).recoverWith { case e => Failure(BadRequestException(e.getMessage))}
        newPremises = substitutedInferencePremises.map(createPremise(_, stepContext, parsingContext))
      } yield Step.NewAssert.Premise.Expansion(oldPremise.statement, inference, newPremises, substitutions)
    }

    modifyStep[Step.NewAssert](bookKey, chapterKey, theoremKey, stepPath) { (book, chapter, theorem, oldStep) =>
      implicit val parsingContext: ParsingContext = getStepParsingContext(book, chapter, theorem, oldStep)
      oldStep.tryUpdatePremiseAtPath(premisePath.indexes, updatePremise(_, oldStep.context)).orNotFound(s"Premise $premisePath not found").flatten
    }.toResponseEntity
  }

  private def findTheorem(bookKey: String, chapterKey: String, theoremKey: String): Try[(Book, Chapter, Theorem)] = {
    for {
      book <- bookService.books.find(_.key.value == bookKey).orNotFound(s"Book $bookKey")
      chapter <- book.chapters.find(_.key.value == chapterKey).orNotFound(s"Chapter $chapterKey")
      theorem <- chapter.entries.ofType[Theorem].find(_.key.value == theoremKey).orNotFound(s"Theorem $theoremKey")
    } yield (book, chapter, theorem)
  }

  private def findStep[T <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, stepReference: PathData): Try[(Book, Chapter, Theorem, T)] = {
    for {
      (book, chapter, theorem) <- findTheorem(bookKey, chapterKey, theoremKey)
      rawStep <- theorem.findStep(stepReference.indexes).orNotFound(s"Step $stepReference")
      step <- rawStep.asOptionalInstanceOf[T].orBadRequest(s"Step was not ${classTag[T].runtimeClass.getName}")
    } yield {
      (book, chapter, theorem, step)
    }
  }

  private def modifyTheorem(bookKey: String, chapterKey: String, theoremKey: String)(f: (Book, Chapter, Theorem) => Try[Theorem]): Try[Theorem] = {
    bookService.modifyEntry[Theorem, Theorem](bookKey, chapterKey, theoremKey) { (_, book, chapter, theorem) =>
      f(book, chapter, theorem).map(t => (t, t))
    }
  }

  private def modifyStep[TStep <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, stepReference: PathData)(f: (Book, Chapter, Theorem, TStep) => Try[Step]): Try[Theorem] = {
    modifyTheorem(bookKey, chapterKey, theoremKey) { (book, chapter, theorem) =>
      theorem.tryModifyStep(stepReference.indexes, oldStep => {
        for {
          oldTStep <- oldStep.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
          newStep <- f(book, chapter, theorem, oldTStep)
        } yield newStep
      }).orNotFound(s"Step $stepReference").flatten
    }
  }

  private def getTheoremParsingContext(book: Book, chapter: Chapter, theorem: Theorem): ParsingContext = {
    val previousChapters = book.chapters.takeWhile(_ != chapter)
    val previousEntries = chapter.entries.takeWhile(_ != theorem)
    ParsingContext(
      book.dependencies.transitive.inferences ++ previousChapters.flatMap(_.inferences) ++ previousEntries.ofType[Inference],
      book.dependencies.transitive.statementDefinitions ++ previousChapters.flatMap(_.statementDefinitions) ++ previousEntries.ofType[StatementDefinition],
      book.dependencies.transitive.termDefinitions ++ previousChapters.flatMap(_.termDefinitions) ++ previousEntries.ofType[TermDefinition],
      book.termVariableNames.toSet,
      Nil)
  }

  private def getStepParsingContext(book: Book, chapter: Chapter, theorem: Theorem, step: Step.Target): ParsingContext = {
    getStepParsingContext(book, chapter, theorem, step.context)
  }

  private def getStepParsingContext(book: Book, chapter: Chapter, theorem: Theorem, step: Step.NewAssert): ParsingContext = {
    getStepParsingContext(book, chapter, theorem, step.context)
  }

  private def getStepParsingContext(book: Book, chapter: Chapter, theorem: Theorem, stepContext: StepContext): ParsingContext = {
    getTheoremParsingContext(book, chapter, theorem).copy(parameterLists = stepContext.boundVariableLists.map(_.zipWithIndex))
  }

  private def findInference(inferenceId: String)(implicit parsingContext: ParsingContext): Try[Inference.Summary] = {
    parsingContext.inferences.find(_.id == inferenceId).map(_.summary).orBadRequest(s"Invalid inference $inferenceId")
  }

  private def createPremise(target: Statement, stepContext: StepContext, parsingContext: ParsingContext): NewAssert.Premise = {
    ProofHelper.findPremise(target, stepContext, parsingContext).getOrElse(NewAssert.Premise.Pending(target))
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

object TheoremController {
  case class InferenceSuggestion(
    inference: Inference,
    requiredSubstitutions: Substitutions.Required,
    substitutions: Seq[Substitutions])
}
