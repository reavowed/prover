package net.prover.controllers

import net.prover.controllers.ChapterController._
import net.prover.controllers.models.ChapterProps._
import net.prover.controllers.models._
import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.definitions.Definitions
import net.prover.model.entries._
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Failure, Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}"))
class ChapterController @Autowired() (val bookService: BookService) extends BookModification with ParameterValidation with ReactViews {

  private def getChapterProps(books: Seq[Book], definitions: Definitions, book: Book, bookKey: String, chapter: Chapter, chapterKey: String): ChapterProps = {
    val chaptersWithKeys = BookService.getChaptersWithKeys(book)
    val index = chaptersWithKeys.findIndexWhere(_._1 == chapter).getOrElse(throw new Exception("Book somehow didn't exist"))
    val previous = chaptersWithKeys.lift(index - 1).map { case (c, key) => LinkSummary(c.title, BookService.getChapterUrl(bookKey, key)) }
    val next = chaptersWithKeys.lift(index + 1).map { case (c, key) => LinkSummary(c.title, BookService.getChapterUrl(bookKey, key)) }
    implicit val entryContext = EntryContext.forChapterInclusive(books, book, chapter)

    val entrySummaries = BookService.getEntriesWithKeys(chapter)
      .map(_.mapRight(key => BookService.getEntryUrl(bookKey, chapterKey, key)))
      .mapCollect { case (entry, url) =>
        entry match {
          case axiom: Axiom =>
            Some(EntryProps("axiom", url, axiom.title, ChapterProps.InferenceSummaryForChapter(axiom, definitions)))
          case theorem: Theorem =>
            Some(EntryProps("theorem", url, theorem.title, ChapterProps.InferenceSummaryForChapter(theorem, definitions)))
          case statementDefinition: StatementDefinitionEntry =>
            Some(EntryProps("statementDefinition", url, statementDefinition.title, statementDefinition))
          case termDefinition: TermDefinitionEntry =>
            Some(EntryProps("statementDefinition", url, termDefinition.title, termDefinition))
          case typeDefinition: TypeDefinition =>
            Some(EntryProps("typeDefinition", url, typeDefinition.title, typeDefinition))
          case typeQualifierDefinition: TypeQualifierDefinition =>
            Some(EntryProps("typeQualifierDefinition", url, typeQualifierDefinition.title, typeQualifierDefinition))
          case propertyDefinition: PropertyDefinitionOnType =>
            Some(EntryProps("propertyDefinition", url, propertyDefinition.title, propertyDefinition))
          case relatedObjectDefinition: RelatedObjectDefinition =>
            Some(EntryProps("relatedObjectDefinition", url, relatedObjectDefinition.title, relatedObjectDefinition))
          case standalonePropertyDefinition: StandalonePropertyDefinition =>
            Some(EntryProps("standalonePropertyDefinition", url, standalonePropertyDefinition.title, standalonePropertyDefinition))
          case comment: Comment =>
            Some(EntryProps("comment", url, None, comment.text))
          case _ =>
            Some(EntryProps("placeholder", url, None, None))
        }
      }
    ChapterProps(
      chapter.title,
      BookService.getChapterUrl(bookKey, chapterKey),
      LinkSummary(book.title, BookService.getBookUrl(bookKey)),
      chapter.summary,
      entrySummaries,
      previous,
      next,
      DefinitionSummary.getAllFromContext(entryContext),
      TypeDefinitionSummary.getAllFromContext(entryContext),
      StandalonePropertyDefinitionSummary.getAllFromContext(entryContext),
      entryContext.availableEntries.ofType[DisplayShorthand],
      DefinitionSummary.getDefinitionShorthandsFromContext(entryContext))
  }

  @GetMapping(produces = Array("text/html;charset=UTF-8"))
  def getChapter(@PathVariable("bookKey") bookKey: String, @PathVariable("chapterKey") chapterKey: String): ResponseEntity[_] = {
    val (books, definitions) = bookService.booksAndDefinitions
    (for {
      book <- bookService.findBook(books, bookKey)
      chapter <- bookService.findChapter(book, chapterKey)
    } yield {
      createReactView("Chapter", getChapterProps(books, definitions, book, bookKey, chapter, chapterKey))
    }).toResponseEntity
  }

  @PutMapping(value = Array("/title"), produces = Array("application/json;charset=UTF-8"))
  def updateTitle(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTitle: String
  ): ResponseEntity[_] = {
    bookService.modifyChapter[Identity](bookKey, chapterKey, (_, _, _, chapter) => {
      Success(chapter.copy(title = newTitle))
    }).map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, BookService.getChaptersWithKeys(book).find(_._1.title == newTitle).get._2) }.toResponseEntity
  }

  @PostMapping(value = Array("/theorems"), produces = Array("application/json;charset=UTF-8"))
  def createTheorem(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTheoremDefinition: NewTheoremModel
  ): ResponseEntity[_] = {
    bookService.addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      for {
        name <- getMandatoryString(newTheoremDefinition.name, "Theorem name")
        variableDefinitions <- getVariableDefinitions(newTheoremDefinition.statementVariableDefinitions, newTheoremDefinition.termVariableDefinitions)
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
    bookService.addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      val name = getOptionalString(newStatementDefinition.name)
      val shorthand = getOptionalString(newStatementDefinition.shorthand)
      val attributes = getWords(newStatementDefinition.attributes)
      for {
        symbol <- getMandatoryString(newStatementDefinition.symbol, "Symbol")
        boundVariableNamesAndComponentTypes <- ExpressionDefinitionEntry.rawBoundVariablesAndComponentTypesParser.parseFromString(newStatementDefinition.components, "components").recoverWithBadRequest
        boundVariableNames = boundVariableNamesAndComponentTypes._1
        componentTypes = boundVariableNamesAndComponentTypes._2
        expressionParsingContext = ExpressionParsingContext.forComponentTypes(componentTypes)
        definition <- newStatementDefinition.definition.filter(_.nonEmpty).map(d => Statement.parser(expressionParsingContext.addInitialParameter("_")).parseFromString(d, "definition").recoverWithBadRequest).swap
        format <- getFormat(newStatementDefinition.format, symbol, boundVariableNames, componentTypes)
        newStatementDefinition = StatementDefinitionEntry(
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
    bookService.addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      val name = getOptionalString(newTermDefinition.name)
      val shorthand = getOptionalString(newTermDefinition.shorthand)
      val attributes = getWords(newTermDefinition.attributes)
      for {
        symbol <- getMandatoryString(newTermDefinition.symbol, "Symbol")
        disambiguator <- getOptionalSingleWord(newTermDefinition.disambiguator, "Disambiguator")
        boundVariableNamesAndComponentTypes <- ExpressionDefinitionEntry.rawBoundVariablesAndComponentTypesParser.parseFromString(newTermDefinition.components, "components").recoverWithBadRequest
        boundVariableNames = boundVariableNamesAndComponentTypes._1
        componentTypes = boundVariableNamesAndComponentTypes._2
        expressionParsingContext = ExpressionParsingContext.forComponentTypes(componentTypes)
        definition <- Statement.parser(expressionParsingContext.addInitialParameter("_")).parseFromString(newTermDefinition.definition, "definition").recoverWithBadRequest
        format <- getFormat(newTermDefinition.format, symbol, boundVariableNames, componentTypes)
        premises <- newTermDefinition.premises.mapWithIndex((str, index) => Statement.parser(expressionParsingContext).parseFromString(str, s"premise ${index + 1}")).recoverWithBadRequest
        newTerm = TermDefinitionEntry(
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

  @PostMapping(value = Array("/typeDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createTypeDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTypeDefinition: NewTypeDefinitionModel
  ): ResponseEntity[_] = {
    bookService.addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      val name = getOptionalString(newTypeDefinition.name)
      for {
        symbol <- getMandatoryString(newTypeDefinition.symbol, "Symbol")
        defaultTermName <- getMandatoryString(newTypeDefinition.defaultTermName, "Default term name")
        qualifier <- getOptionalQualifier(newTypeDefinition.qualifierTermNames, newTypeDefinition.qualifierFormat)
        expressionParsingContext = ExpressionParsingContext.forTypeDefinition(defaultTermName +: qualifier.defaultTermNames)
        definition <- Statement.parser(expressionParsingContext).parseFromString(newTypeDefinition.definition, "definition").recoverWithBadRequest
        newTypeDefinition = TypeDefinition(
          symbol,
          defaultTermName,
          qualifier,
          name,
          definition)
      } yield newTypeDefinition
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/typeQualifierDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createTypeQualifierDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTypeQualifierDefinition: NewTypeQualifierDefinitionModel
  ): ResponseEntity[_] = {
    bookService.addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      val name = getOptionalString(newTypeQualifierDefinition.name)
      for {
        symbol <- getMandatoryString(newTypeQualifierDefinition.symbol, "Symbol")
        parentType <- entryContext.typeDefinitions.get(newTypeQualifierDefinition.parentType).orBadRequest(s"Unknown type '${newTypeQualifierDefinition.parentType}'")
        qualifier <- getQualifier(newTypeQualifierDefinition.qualifierTermNames, newTypeQualifierDefinition.qualifierFormat)
        expressionParsingContext = ExpressionParsingContext.forTypeDefinition(parentType.defaultTermName +: qualifier.defaultTermNames)
        definition <- Statement.parser(expressionParsingContext).parseFromString(newTypeQualifierDefinition.definition, "definition").recoverWithBadRequest
        conjunctionDefinition <- entryContext.conjunctionDefinitionOption.orBadRequest("Cannot create property without conjunction")
        newTypeQualifierDefinition = TypeQualifierDefinition(
          symbol,
          parentType,
          qualifier,
          name,
          definition,
          conjunctionDefinition)
      } yield newTypeQualifierDefinition
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/propertyDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createPropertyDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newPropertyDefinition: NewPropertyDefinitionModel
  ): ResponseEntity[_] = {
    bookService.addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      val name = getOptionalString(newPropertyDefinition.name)
      for {
        symbol <- getMandatoryString(newPropertyDefinition.symbol, "Symbol")
        parentType <- entryContext.typeDefinitions.get(newPropertyDefinition.parentType).orBadRequest(s"Unknown type '${newPropertyDefinition.parentType}'")
        requiredParentQualifier <- getOptionalParentQualifier(parentType, newPropertyDefinition.requiredParentQualifier)
        requiredParentObjects <- getParentObjects(parentType, newPropertyDefinition.requiredParentObjects)
        adapter <- getOptionalAdapter(newPropertyDefinition.ownTermNames, newPropertyDefinition.parentTerms, (requiredParentQualifier.map(_.qualifier) orElse parentType.defaultQualifier).defaultTermNames)
        conjunctionDefinition <- entryContext.conjunctionDefinitionOption.orBadRequest("Cannot create property without conjunction")
        qualifierTermNames = PropertyDefinitionOnType.getParentConditionAndQualifierTermNames(parentType, adapter, requiredParentQualifier, requiredParentObjects, conjunctionDefinition )._2
        expressionParsingContext = requiredParentObjects.addParametersToParsingContext(ExpressionParsingContext.forTypeDefinition(qualifierTermNames))
        definingStatement <- Statement.parser(expressionParsingContext).parseFromString(newPropertyDefinition.definingStatement, "definition").recoverWithBadRequest
        newPropertyDefinition = PropertyDefinitionOnType(
          symbol,
          parentType,
          requiredParentQualifier,
          requiredParentObjects,
          adapter,
          name,
          definingStatement,
          conjunctionDefinition)
      } yield newPropertyDefinition
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/relatedObjectDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createRelatedObjectDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newRelatedObjectDefinition: NewRelatedObjectDefinitionModel
  ): ResponseEntity[_] = {
    bookService.addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      val name = getOptionalString(newRelatedObjectDefinition.name)
      for {
        symbol <- getMandatoryString(newRelatedObjectDefinition.symbol, "Symbol")
        defaultTermName <- getMandatoryString(newRelatedObjectDefinition.defaultTermName, "Default term name")
        parentType <- entryContext.typeDefinitions.get(newRelatedObjectDefinition.parentType).orBadRequest(s"Unknown type '${newRelatedObjectDefinition.parentType}'")
        requiredParentQualifier <- getOptionalParentQualifier(parentType, newRelatedObjectDefinition.requiredParentQualifier)
        expressionParsingContext = ExpressionParsingContext.forTypeDefinition(parentType.defaultTermName +: requiredParentQualifier.map(_.qualifier).orElse(parentType.defaultQualifier).defaultTermNames)
        definingStatement <- Statement.parser(expressionParsingContext).parseFromString(newRelatedObjectDefinition.definingStatement, "definition").recoverWithBadRequest
        conjunctionDefinition <- entryContext.conjunctionDefinitionOption.orBadRequest("Cannot create property without conjunction")
        newPropertyDefinition = RelatedObjectDefinition(
          symbol,
          parentType,
          defaultTermName,
          requiredParentQualifier,
          name,
          definingStatement,
          conjunctionDefinition)
      } yield newPropertyDefinition
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/standalonePropertyDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createStandalonePropertyDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newPropertyDefinition: NewStandalonePropertyDefinitionModel
  ): ResponseEntity[_] = {
    bookService.addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      val name = getOptionalString(newPropertyDefinition.name)
      for {
        symbol <- getMandatoryString(newPropertyDefinition.symbol, "Symbol")
        defaultTermName <- getMandatoryString(newPropertyDefinition.defaultTermName, "Default term name")
        expressionParsingContext = ExpressionParsingContext.forTypeDefinition(Seq(defaultTermName))
        definition <- Statement.parser(expressionParsingContext).parseFromString(newPropertyDefinition.definingStatement, "definition").recoverWithBadRequest
        newPropertyDefinition = StandalonePropertyDefinition(
          symbol,
          defaultTermName,
          name,
          definition)
      } yield newPropertyDefinition
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/comments"), produces = Array("application/json;charset=UTF-8"))
  def createComment(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newCommentText: String
  ): ResponseEntity[_] = {
    bookService.addChapterEntry(bookKey, chapterKey) { (_, _, _) =>
      Option(newCommentText.trim).filter(_.nonEmpty).orBadRequest("Comment text must be provided").map(Comment(_))
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PutMapping(value = Array("/{entryKey}/index"), produces = Array("application/json;charset=UTF-8"))
  def moveEntryToIndex(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody newIndex: Int
  ): ResponseEntity[_] = {
    def tryMove(entry: ChapterEntry, previousEntries: Seq[ChapterEntry], nextEntries: Seq[ChapterEntry]): Try[Seq[ChapterEntry]] = {
      previousEntries.takeAndRemainingIfValid(newIndex).map { case (firstEntries, entriesToMoveBefore) =>
        for {
          _ <- findUsage(Seq(entry), entriesToMoveBefore).badRequestIfDefined { case (entryUsing, usedEntry) => s"""Entry "${entryUsing.name}" depends on "${usedEntry.name}""""}
        } yield (firstEntries :+ entry) ++ entriesToMoveBefore ++ nextEntries
      } orElse nextEntries.takeAndRemainingIfValid(newIndex - previousEntries.length).map { case (entriesToMoveAfter, lastEntries) =>
        for {
          _ <- findUsage(entriesToMoveAfter, Seq(entry)).badRequestIfDefined { case (entryUsing, usedEntry) => s"""Entry "${entryUsing.name}" depends on "${usedEntry.name}""""}
        } yield (previousEntries ++ entriesToMoveAfter :+ entry) ++ lastEntries
      } orBadRequest "Invalid index" flatten
    }
    bookService.modifyChapter[Identity](bookKey, chapterKey, (_, _, _, chapter) => {
      for {
        (previousEntries, entry, nextEntries) <- BookService.getEntriesWithKeys(chapter).splitWhere(_._2 == entryKey).orNotFound(s"Entry $entryKey")
        updatedEntries <- tryMove(entry._1, previousEntries.map(_._1), nextEntries.map(_._1))
      } yield chapter.copy(entries = updatedEntries)
    }).map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @DeleteMapping(value = Array("/{entryKey}"), produces = Array("application/json;charset=UTF-8"))
  def deleteEntry(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String
  ): ResponseEntity[_] = {
    def deleteEntry(chapterEntry: ChapterEntry, chapter: Chapter, books: Seq[Book]): Try[Chapter] = {
      findUsage(books, chapterEntry)
        .badRequestIfDefined { case (entryUsing, usedEntry) => s"""Entry "${entryUsing.name}" depends on "${usedEntry.name}""""}
        .map(_ => chapter.copy(entries = chapter.entries.filter(_ != chapterEntry)))
    }

    bookService.modifyChapter[Identity](bookKey, chapterKey, (books, _, _, chapter) =>
      for {
        entry <- bookService.findEntry[ChapterEntry](chapter, entryKey)
        updatedChapter <- deleteEntry(entry, chapter, books)
      } yield updatedChapter
    ).map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }
}

object ChapterController {
  case class NewTheoremModel(
    name: String,
    statementVariableDefinitions: Seq[String],
    termVariableDefinitions: Seq[String],
    premises: Seq[String],
    conclusion: String)
  case class NewStatementDefinitionModel(
    symbol: String,
    components: String,
    name: String,
    format: String,
    definition: Option[String],
    shorthand: String,
    attributes: String)
  case class NewTermDefinitionModel(
    symbol: String,
    components: String,
    disambiguator: String,
    name: String,
    format: String,
    premises: Seq[String],
    definition: String,
    shorthand: String,
    attributes: String)
  case class NewTypeDefinitionModel(
    symbol: String,
    defaultTermName: String,
    qualifierTermNames: String,
    qualifierFormat: String,
    name: String,
    definition: String)
  case class NewTypeQualifierDefinitionModel(
    symbol: String,
    parentType: String,
    qualifierTermNames: String,
    qualifierFormat: String,
    name: String,
    definition: String)
  case class NewPropertyDefinitionModel(
    symbol: String,
    parentType: String,
    requiredParentQualifier: String,
    requiredParentObjects: String,
    name: String,
    definingStatement: String,
    ownTermNames: String,
    parentTerms: String)
  case class NewRelatedObjectDefinitionModel(
    symbol: String,
    parentType: String,
    requiredParentQualifier: String,
    name: String,
    defaultTermName: String,
    definingStatement: String)
  case class NewStandalonePropertyDefinitionModel(
    symbol: String,
    defaultTermName: String,
    name: String,
    definingStatement: String)
}
