package net.prover.controllers

import net.prover.controllers.TheoremController._
import net.prover.controllers.models.{StepDefinition, ValueOrResponseEntity}
import net.prover.model.entries.{StatementDefinition, TermDefinition, Theorem}
import net.prover.model.expressions.Expression
import net.prover.model.proof.{ProvenStatement, Reference, Step, StepContext}
import net.prover.model.{Book, Chapter, DisplayContext, Inference, ParsingContext, Substitutions}
import net.prover.services.BookService
import net.prover.views.ExpressionView
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.core.convert.converter.Converter
import org.springframework.http.{HttpStatus, ResponseEntity}
import org.springframework.stereotype.Component
import org.springframework.web.bind.annotation._
import net.prover.controllers.models.ValueOrResponseEntityConverters._

import scala.reflect._

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{theoremKey}"))
class TheoremController @Autowired() (bookService: BookService) {

  @GetMapping(value = Array("/{stepReference}/namingOptions"), produces = Array("application/json;charset=UTF-8"))
  def getNamingOptions(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: StepReference
  ): ResponseEntity[_] = {
    (for {
      (book, chapter, theorem, _, stepContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference)
      parsingContext = getTheoremParsingContext(book, chapter, theorem)
      namingInferences <- parsingContext.findNamingInferences().orBadRequest("Scoping or deduction statement not available")
    } yield {
      import book.displayContext
      for {
        (_, premise) <- namingInferences
        availableStatement <- stepContext.availableStatements
        if premise.statement.calculateSubstitutions(availableStatement.statement, Substitutions.empty, 0, stepContext.externalDepth).nonEmpty
        summary <- ProvenStatementSummary(availableStatement)
      } yield summary
    }).toResponseEntity
  }

  @GetMapping(value = Array("/{stepReference}/suggestions"), produces = Array("application/json;charset=UTF-8"))
  def suggestInferences(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("theoremKey") theoremKey: String,
    @PathVariable("stepReference") stepReference: StepReference,
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
    @PathVariable("stepReference") stepReference: StepReference,
    @RequestBody definition: StepDefinition
  ): ResponseEntity[_] = {
    (for {
      (book, chapter, theorem, oldStep, stepContext) <- findStep[Step.Target](bookKey, chapterKey, theoremKey, stepReference)
      parsingContext = getTheoremParsingContext(book, chapter, theorem)
      inference <- definition.getInference(parsingContext)
      substitutions <- definition.parseSubstitutions(inference)(parsingContext)
      premiseStatements = inference.substitutePremisesAndValidateConclusion(substitutions, oldStep.statement, stepContext.externalDepth)
    } yield {
      val newStep = Step.NewAssert(
        oldStep.statement,
        inference,
        premiseStatements.map(Step.NewAssert.FloatingPremise.apply),
        substitutions,
        oldStep.reference)
      bookService.modifyEntry[Theorem](bookKey, chapterKey, theoremKey)(_.replaceStep(stepReference.indexes, newStep))
      null
    }).toResponseEntity
  }

  private def findTheorem(bookKey: String, chapterKey: String, theoremKey: String): ValueOrResponseEntity[(Book, Chapter, Theorem)] = {
    (for {
      book <- bookService.books.find(_.key.value == bookKey)
      chapter <- book.chapters.find(_.key.value == chapterKey)
      theorem <- chapter.entries.ofType[Theorem].find(_.key.value == theoremKey)
    } yield (book, chapter, theorem)).orNotFound
  }

  private def findStep[T <: Step : ClassTag](bookKey: String, chapterKey: String, theoremKey: String, stepReference: StepReference): ValueOrResponseEntity[(Book, Chapter, Theorem, T, StepContext)] = {
    for {
      (book, chapter, theorem) <- findTheorem(bookKey, chapterKey, theoremKey)
      (rawStep, stepContext) <- theorem.findStepWithContext(stepReference.indexes).orNotFound
      step <- rawStep.asOptionalInstanceOf[T].orResponseEntity(new ResponseEntity(s"Step was not ${classTag[T].runtimeClass.getName}", HttpStatus.BAD_REQUEST))
    } yield {
      (book, chapter, theorem, step, stepContext)
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

  case class StepReference(indexes: Seq[Int])
  @Component
  class StepReferenceConverter extends Converter[String, StepReference] {
    override def convert(source: String): StepReference = StepReference(source.split('.').map(_.toInt))
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

  case class ProvenStatementSummary(
    reference: String,
    statement: ExpressionSummary)
  object ProvenStatementSummary {
    def apply(provenStatement: ProvenStatement)(implicit displayContext: DisplayContext): Option[ProvenStatementSummary] = {
      provenStatement.reference.asOptionalInstanceOf[Reference.Direct].map { reference =>
        ProvenStatementSummary(
          reference.value,
          ExpressionSummary(provenStatement.statement))
      }
    }
  }
}
