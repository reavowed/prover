package net.prover.controllers

import net.prover.controllers.TheoremController._
import net.prover.controllers.models.StepDefinition
import net.prover.model._
import net.prover.model.entries.{StatementDefinition, TermDefinition, Theorem}
import net.prover.model.expressions.Expression
import net.prover.model.proof.{Step, StepContext}
import net.prover.services.BookService
import net.prover.views.ExpressionView
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.core.convert.converter.Converter
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Component
import org.springframework.web.bind.annotation._

import scala.reflect._
import scala.util.Try

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
      (book, chapter, theorem, step, stepContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference)
    } yield {
      import book.displayContext
      implicit val parsingContext: ParsingContext = getTheoremParsingContext(book, chapter, theorem)
      parsingContext.inferences
        .filter(_.name.toLowerCase.contains(searchText.toLowerCase))
        .mapCollect { inference =>
          val substitutions = inference.conclusion.calculateSubstitutions(step.statement, Substitutions.empty, 0, stepContext.externalDepth)
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
    modifyStep[Step.Target, Unit](bookKey, chapterKey, theoremKey, stepReference) { (book, chapter, theorem, oldStep, stepContext) =>
      val parsingContext: ParsingContext = getTheoremParsingContext(book, chapter, theorem)
      for {
        inference <- definition.getInference(parsingContext)
        substitutions <- definition.parseSubstitutions(inference)(parsingContext)
        premiseStatements = inference.substitutePremisesAndValidateConclusion(substitutions, oldStep.statement, stepContext.externalDepth)
      } yield {
        (Step.NewAssert(
          oldStep.statement,
          inference,
          premiseStatements.map(Step.NewAssert.FloatingPremise.apply),
          substitutions,
          oldStep.reference),
          ())
      }
    }.toResponseEntity
  }

  @PutMapping(value = Array("/{stepReference}/premises/{premiseIndex}/statement/{expressionPath}/boundVariables/{boundVariableIndex}"))
  def editBoundVariableName(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepPath: PathData,
    @PathVariable("premiseIndex") premiseIndex: Int,
    @PathVariable("expressionPath") expressionPath: PathData,
    @PathVariable("boundVariableIndex") boundVariableIndex: Int,
    @RequestBody newBoundVariableName: String
  ): ResponseEntity[_] = {
    modifyStep[Step.NewAssert, ExpressionSummary](bookKey, chapterKey, theoremKey, stepPath) { (book, _, _, oldStep, _) =>
      import book.displayContext
      for {
        (newPremises, newPremise) <- oldStep.premises.updateAtIndexIfDefinedWithResult(premiseIndex) { p =>
          p.updateStatement(_.renameBoundVariable(newBoundVariableName, boundVariableIndex, expressionPath.indexes).orNotFound(s"Bound variable $boundVariableIndex at ${expressionPath.indexes.mkString(".")}")).map(x => (x, x))
        }.orNotFound(s"Premise $premiseIndex").flatten
      } yield (oldStep.copy(premises = newPremises), ExpressionSummary(newPremise.statement))
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
      (rawStep, stepContext) <- theorem.findStepWithContext(stepReference.indexes).orNotFound(s"Step ${stepReference.indexes.mkString(".")}")
      step <- rawStep.asOptionalInstanceOf[T].orBadRequest(s"Step was not ${classTag[T].runtimeClass.getName}")
    } yield {
      (book, chapter, theorem, step, stepContext)
    }
  }

  private def modifyStep[TStep <: Step : ClassTag, TResult](bookKey: String, chapterKey: String, theoremKey: String, stepReference: PathData)(f: (Book, Chapter, Theorem, TStep, StepContext) => Try[(Step, TResult)]): Try[TResult] = {
    bookService.modifyEntry[Theorem, TResult](bookKey, chapterKey, theoremKey) { (_, book, chapter, theorem) =>
      for {
        (rawStep, stepContext) <- theorem.findStepWithContext(stepReference.indexes).orNotFound(s"Step ${stepReference.indexes.mkString(".")}")
        oldStep <- rawStep.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
        (newStep, result) <- f(book, chapter, theorem, oldStep, stepContext)
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

  case class PathData(indexes: Seq[Int])
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
