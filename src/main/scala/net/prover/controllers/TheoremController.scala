package net.prover.controllers

import net.prover.controllers.TheoremController._
import net.prover.controllers.models.StepDefinition
import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.entries.{StatementDefinition, TermDefinition, Theorem}
import net.prover.model.expressions.{Expression, Statement}
import net.prover.model.proof.Step.NewAssert
import net.prover.model.proof.{ProvenStatement, Step, StepContext}
import net.prover.services.BookService
import net.prover.views.{ExpressionView, StepView}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.core.convert.converter.Converter
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Component
import org.springframework.web.bind.annotation._

import scala.reflect._
import scala.util.{Failure, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}"))
class TheoremController @Autowired() (bookService: BookService) {
  @GetMapping(value = Array("/{stepReference}/suggestions"), produces = Array("application/json;charset=UTF-8"))
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
      import book.displayContext
      implicit val parsingContext: ParsingContext = getTheoremParsingContext(book, chapter, theorem)
      parsingContext.inferences
        .filter(_.name.toLowerCase.contains(searchText.toLowerCase))
        .mapCollect { inference =>
          val substitutions = inference.conclusion.calculateSubstitutions(step.statement, Substitutions.empty, 0, step.context.externalDepth)
          if (substitutions.nonEmpty)
            Some(InferenceSuggestion(InferenceSummary(inference), substitutions.map(SubstitutionsSummary(_, inference.requiredSubstitutions))))
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
    modifyStep[Step.Target, Unit](bookKey, chapterKey, theoremKey, stepReference) { (book, chapter, theorem, oldStep) =>
      implicit val stepContext: StepContext = oldStep.context
      implicit val parsingContext: ParsingContext = getTheoremParsingContext(book, chapter, theorem)
      for {
        inference <- findInference(definition.inferenceId)
        substitutions <- definition.parseSubstitutions(inference)
        premiseStatements <- Try(inference.substitutePremisesAndValidateConclusion(substitutions, oldStep.statement, stepContext.externalDepth)).recoverWith { case e => Failure(BadRequestException(e.getMessage))}
      } yield {
        (Step.NewAssert(
          oldStep.statement,
          inference,
          premiseStatements.map(createPremise(_, stepContext)),
          substitutions,
          oldStep.context),
          ())
      }
    }.toResponseEntity
  }

  @PutMapping(value = Array("/{stepReference}/premises/{premisePath}/statement/{expressionPath}/boundVariables/{boundVariableIndex}"))
  def editBoundVariableName(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepPath: PathData,
    @PathVariable("premisePath") premisePath: PathData,
    @PathVariable("expressionPath") expressionPath: PathData,
    @PathVariable("boundVariableIndex") boundVariableIndex: Int,
    @RequestBody newBoundVariableName: String
  ): ResponseEntity[_] = {
    import Step.NewAssert._
    def updatePremise(oldPremise: Premise): Try[Premise] = {
      for {
        oldPendingPremise <- oldPremise.asOptionalInstanceOf[Premise.Pending].orBadRequest(s"Premise $premisePath is not pending")
        newStatement <- oldPendingPremise.statement.renameBoundVariable(newBoundVariableName, boundVariableIndex, expressionPath.indexes).orNotFound(s"Bound variable $boundVariableIndex at $expressionPath")
      } yield oldPendingPremise.copy(statement = newStatement)
    }
    modifyStep[Step.NewAssert, UpdatedStep](bookKey, chapterKey, theoremKey, stepPath) { (book, _, _, oldStep) =>
      import book.displayContext
      for {
        updatedStep <- oldStep.tryUpdatePremiseAtPath(premisePath.indexes, updatePremise).orNotFound(s"Premise $premisePath").flatten
      } yield (updatedStep, UpdatedStep(StepView(updatedStep, stepPath.indexes).toString()))
    }.toResponseEntity
  }

  case class PremiseOption(path: Seq[Int], expansions: Seq[InferenceSummary])
  @GetMapping(value = Array("/{stepReference}/premiseOptions"))
  def stepOptions(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: PathData
  ): ResponseEntity[_] = {
    findStep[Step.NewAssert](bookKey, chapterKey, theoremKey, stepReference).map { case (book, chapter, theorem, step) =>
      val parsingContext = getTheoremParsingContext(book, chapter, theorem)
      val expansionInferences = parsingContext.inferences.filter(_.rearrangementType == Inference.RearrangementType.Expansion)
      step.pendingPremises.map { case (path, p) =>
        val matchingExpansions = expansionInferences.filter(_.conclusion.calculateSubstitutions(p.statement, Substitutions.empty, 0, step.context.externalDepth).nonEmpty)
        PremiseOption(path, matchingExpansions.map(InferenceSummary.apply))
      }
    }.toResponseEntity
  }

  case class UpdatedStep(html: String)
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
        newPremises = substitutedInferencePremises.map(createPremise(_, stepContext))
      } yield Step.NewAssert.Premise.Rearrangement(oldPremise.statement, inference, newPremises, substitutions)
    }

    modifyStep[Step.NewAssert, UpdatedStep](bookKey, chapterKey, theoremKey, stepPath) { (book, chapter, theorem, oldStep) =>
      import book.displayContext
      implicit val parsingContext: ParsingContext = getTheoremParsingContext(book, chapter, theorem)
      for {
        updatedStep <- oldStep.tryUpdatePremiseAtPath(premisePath.indexes, updatePremise(_, oldStep.context)).orNotFound(s"Premise $premisePath not found").flatten
      } yield (updatedStep, UpdatedStep(StepView(updatedStep).toString))
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

  private def modifyStep[TStep <: Step : ClassTag, TResult](bookKey: String, chapterKey: String, theoremKey: String, stepReference: PathData)(f: (Book, Chapter, Theorem, TStep) => Try[(Step, TResult)]): Try[TResult] = {
    bookService.modifyEntry[Theorem, TResult](bookKey, chapterKey, theoremKey) { (_, book, chapter, theorem) =>
      for {
        rawStep <- theorem.findStep(stepReference.indexes).orNotFound(s"Step $stepReference")
        oldStep <- rawStep.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
        (newStep, result) <- f(book, chapter, theorem, oldStep)
      } yield (theorem.replaceStep(stepReference.indexes, newStep), result)
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

  private def findInference(inferenceId: String)(implicit parsingContext: ParsingContext): Try[Inference] = {
    parsingContext.inferences.find(_.id == inferenceId).orBadRequest(s"Invalid inference $inferenceId")
  }

  private def createPremise(target: Statement, stepContext: StepContext): NewAssert.Premise.Leaf = {
    stepContext.findProvenStatement(target) match {
      case Some(ProvenStatement(statement, reference)) =>
        NewAssert.Premise.Given(statement, reference)
      case None =>
        NewAssert.Premise.Pending(target)
    }
  }

  case class PathData(indexes: Seq[Int]) {
   override def toString: String = indexes.mkString(".")
  }
  @Component
  class StepReferenceConverter extends Converter[String, PathData] {
    override def convert(source: String): PathData = PathData(source.split('.').map(_.toInt))
  }
}

object TheoremController {
  case class InferenceSuggestion(
    inference: InferenceSummary,
    substitutions: Seq[SubstitutionsSummary])

  case class InferenceSummary(
    name: String,
    id: String,
    url: String)
  object InferenceSummary {
    def apply(inference: Inference): InferenceSummary = {
      InferenceSummary(
        inference.name,
        inference.id,
        inference.entryKey.url)
    }
  }

  case class SubstitutionsSummary(
    statements: Map[String, Option[ExpressionSummary]],
    terms: Map[String, Option[ExpressionSummary]],
    predicates: Map[String, Option[ExpressionSummary]],
    functions: Map[String, Option[ExpressionSummary]])
  object SubstitutionsSummary {
    def apply(
      substitutions: Substitutions,
      requiredSubstitutions: Substitutions.Required)(
      implicit displayContext: DisplayContext
    ): SubstitutionsSummary = SubstitutionsSummary(
      requiredSubstitutions.statements.map(s => s -> substitutions.statements.get(s).map(ExpressionSummary.apply)).toMap,
      requiredSubstitutions.terms.map(s => s -> substitutions.terms.get(s).map(ExpressionSummary.apply)).toMap,
      requiredSubstitutions.predicates.map(s => s -> substitutions.predicates.get(s).map(ExpressionSummary.apply)).toMap,
      requiredSubstitutions.functions.map(s => s -> substitutions.functions.get(s).map(ExpressionSummary.apply)).toMap)
  }

  case class ExpressionSummary(
    serialized: String,
    html: String)
  object ExpressionSummary {
    def apply(
      expression: Expression)(
      implicit displayContext: DisplayContext
    ): ExpressionSummary = ExpressionSummary(
      expression.serialized,
      ExpressionView(expression).toString())
  }
}
