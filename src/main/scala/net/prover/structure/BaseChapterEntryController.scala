package net.prover.structure

import net.prover._
import net.prover.exceptions.BadRequestException
import net.prover.model.ExpressionParsingContext
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step
import net.prover.structure.datatransfer._
import net.prover.structure.model.entries.{CompoundExpressionDefinitionEntry, CompoundStatementDefinitionEntry, CompoundTermDefinitionEntry, Theorem}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.{PathVariable, PostMapping, RequestBody, RequestMapping, RestController}

import scala.util.{Failure, Success}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}"))
class BaseChapterEntryController @Autowired() (val bookService: BookService) extends ChapterControllerBase {
  @PostMapping(value = Array("/theorems"), produces = Array("application/json;charset=UTF-8"))
  def createTheorem(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTheoremDefinition: NewTheoremModel
  ): ResponseEntity[_] = {
    addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      for {
        name <- getMandatoryString(newTheoremDefinition.name, "Theorem name")
        variableDefinitions <- getVariableDefinitions(newTheoremDefinition.variableDefinitions)
        expressionParsingContext = ExpressionParsingContext.withDefinitions(variableDefinitions)
        premises <- getPremises(newTheoremDefinition.premises)(expressionParsingContext)
        conclusion <- getStatement(newTheoremDefinition.conclusion, "conclusion")(expressionParsingContext)
        newTheorem = Theorem(
          name,
          variableDefinitions,
          premises,
          conclusion,
          Seq(Theorem.Proof(Seq(Step.Target(conclusion)))))
        existingTheoremOption = entryContext.allInferences.find(_.id == newTheorem.id)
        _ <- existingTheoremOption match {
          case Some(_) =>
            Failure(BadRequestException("An inference with these premises and conclusion already exists"))
          case None =>
            Success(newTheorem)
        }
      } yield newTheorem
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/statementDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createStatementDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newStatementDefinition: NewStatementDefinitionModel
  ): ResponseEntity[_] = {
    addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      val name = getOptionalString(newStatementDefinition.name)
      val shorthand = getOptionalString(newStatementDefinition.shorthand)
      val attributes = getWords(newStatementDefinition.attributes)
      for {
        symbol <- getMandatoryString(newStatementDefinition.symbol, "Symbol")
        boundVariableNamesAndComponentTypes <- CompoundExpressionDefinitionEntry.rawBoundVariablesAndComponentTypesParser.parseFromString(newStatementDefinition.components, "components").recoverWithBadRequest
        boundVariableNames = boundVariableNamesAndComponentTypes._1
        componentTypes = boundVariableNamesAndComponentTypes._2
        expressionParsingContext = ExpressionParsingContext.forComponentTypes(componentTypes)
        definition <- newStatementDefinition.definition.filter(_.nonEmpty).map(d => Statement.parser(expressionParsingContext.addInitialParameter("_")).parseFromString(d, "definition").recoverWithBadRequest).swap
        format <- getFormat(newStatementDefinition.format, symbol, boundVariableNames, componentTypes)
        newStatementDefinition = CompoundStatementDefinitionEntry(
          symbol,
          boundVariableNames,
          componentTypes,
          name,
          format,
          definition,
          shorthand,
          attributes)
      } yield newStatementDefinition
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/termDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createTermDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTermDefinition: NewTermDefinitionModel
  ): ResponseEntity[_] = {
    addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      val name = getOptionalString(newTermDefinition.name)
      val shorthand = getOptionalString(newTermDefinition.shorthand)
      val attributes = getWords(newTermDefinition.attributes)
      for {
        symbol <- getMandatoryString(newTermDefinition.symbol, "Symbol")
        disambiguator <- getOptionalSingleWord(newTermDefinition.disambiguator, "Disambiguator")
        boundVariableNamesAndComponentTypes <- CompoundExpressionDefinitionEntry.rawBoundVariablesAndComponentTypesParser.parseFromString(newTermDefinition.components, "components").recoverWithBadRequest
        boundVariableNames = boundVariableNamesAndComponentTypes._1
        componentTypes = boundVariableNamesAndComponentTypes._2
        expressionParsingContext = ExpressionParsingContext.forComponentTypes(componentTypes)
        definition <- Statement.parser(expressionParsingContext.addInitialParameter("_")).parseFromString(newTermDefinition.definition, "definition").recoverWithBadRequest
        format <- getFormat(newTermDefinition.format, symbol, boundVariableNames, componentTypes)
        premises <- newTermDefinition.premises.mapWithIndex((str, index) => Statement.parser(expressionParsingContext).parseFromString(str, s"premise ${index + 1}")).recoverWithBadRequest
        newTerm = CompoundTermDefinitionEntry(
          symbol,
          boundVariableNames,
          componentTypes,
          disambiguator,
          name,
          format,
          premises,
          definition,
          shorthand,
          attributes,
          Nil)
      } yield newTerm
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }
}
