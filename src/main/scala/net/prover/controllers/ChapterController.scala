package net.prover.controllers

import net.prover.controllers.ChapterController._
import net.prover.controllers.models.ChapterProps._
import net.prover.controllers.models.{ChapterProps, DefinitionSummary, LinkSummary, StandalonePropertyDefinitionSummary, TypeDefinitionSummary}
import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.definitions.Definitions
import net.prover.model.entries.ExpressionDefinition.ComponentType
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
          case axiom: entries.Axiom =>
            import axiom._
            Some(AxiomPropsForChapter(name, url, premises, conclusion))
          case theorem: entries.Theorem =>
            import theorem._
            Some(TheoremPropsForChapter(name, url, premises, conclusion, definitions.isInferenceComplete(theorem)))
          case statementDefinition: entries.StatementDefinition =>
            import statementDefinition._
            Some(StatementDefinitionPropsForChapter(defaultValue, url, shorthand, definingStatement))
          case termDefinition: entries.TermDefinition =>
            import termDefinition._
            Some(TermDefinitionPropsForChapter(defaultValue, url, shorthand, definingStatement, premises))
          case typeDefinition: entries.TypeDefinition =>
            import typeDefinition._
            Some(TypeDefinitionPropsForChapter(symbol, url, defaultTermName, otherComponentTypes.map(_.name), definingStatement))
          case propertyDefinition: entries.PropertyDefinitionOnType =>
            import propertyDefinition._
            Some(PropertyDefinitionPropsForChapter(name, url, defaultTermName, parentType.symbol, parentComponentTypes.map(_.name), definingStatement))
          case standalonePropertyDefinition: entries.StandalonePropertyDefinition =>
            import standalonePropertyDefinition._
            Some(StandalonePropertyDefinitionPropsForChapter(name, url, defaultTermName, otherComponentTypes.map(_.name), definingStatement))
          case comment: entries.Comment =>
            import comment._
            Some(CommentPropsForChapter(text, url))
          case _ =>
            Some(PlaceholderPropsForChapter(url))
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
      getDefinitionShorthands(entryContext))
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

  @GetMapping(value = Array("/{entryKey}"), produces = Array("text/html;charset=UTF-8"))
  def getEntry(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String
  ): ResponseEntity[_] = {
    val (books, definitions) = bookService.booksAndDefinitions
    (for {
      book <- bookService.findBook(books, bookKey)
      chapter <- bookService.findChapter(book, chapterKey)
      entriesWithKeys = BookService.getEntriesWithKeys(chapter).mapCollect(_.optionMapLeft(_.asOptionalInstanceOf[ChapterEntry.Standalone]))
      entry <- bookService.findEntry[ChapterEntry](entriesWithKeys, entryKey)
      entryContext = EntryContext.forEntry(books, book, chapter, entry).addEntry(entry)
      (viewName, baseProps) <- entry match {
        case axiom: Axiom =>
          Success(("Axiom", Map("axiom" -> axiom)))
        case theorem: Theorem =>
          Success(("Theorem", Map("theorem" -> theorem, "inferences" -> BookService.getInferenceLinks(theorem.referencedInferenceIds, books, definitions))))
        case statementDefinition: StatementDefinition =>
          Success(("StatementDefinition", Map("definition" -> statementDefinition)))
        case termDefinition: TermDefinition =>
          Success(("TermDefinition", Map("definition" -> termDefinition)))
        case typeDefinition: TypeDefinition =>
          Success(("TypeDefinition", Map("definition" -> typeDefinition)))
        case _ =>
          Failure(BadRequestException(s"Cannot view ${entry.getClass.getSimpleName}"))
      }
    } yield {
      val provingContext = ProvingContext(entryContext, definitions)
      val index = entriesWithKeys.findIndexWhere(_._1 == entry).getOrElse(throw new Exception("Book somehow didn't exist"))
      val previous = entriesWithKeys.lift(index - 1).map { case (c, key) => LinkSummary(c.title, key) }
      val next = entriesWithKeys.lift(index + 1).map { case (c, key) => LinkSummary(c.title, key) }
      createReactView(
        viewName,
        baseProps ++ Map(
          "url" -> BookService.getEntryUrl(bookKey, chapterKey, entryKey),
          "bookLink" -> LinkSummary(book.title, BookService.getBookUrl(bookKey)),
          "chapterLink" -> LinkSummary(chapter.title, BookService.getChapterUrl(bookKey, chapterKey)),
          "previous" -> previous,
          "next" -> next,
          "usages" -> getInferenceUsages(entry, books),
          "definitions" -> DefinitionSummary.getAllFromContext(entryContext),
          "typeDefinitions" -> TypeDefinitionSummary.getAllFromContext(entryContext),
          "standalonePropertyDefinitions" -> StandalonePropertyDefinitionSummary.getAllFromContext(entryContext),
          "displayShorthands" -> entryContext.availableEntries.ofType[DisplayShorthand],
          "binaryRelations" -> getBinaryRelations(provingContext),
          "definitionShorthands" -> getDefinitionShorthands(entryContext)))
    }).toResponseEntity
  }

  @PostMapping(value = Array("/theorems"), produces = Array("application/json;charset=UTF-8"))
  def createTheorem(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTheoremDefininition: NewTheoremModel
  ): ResponseEntity[_] = {
    bookService.addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
      for {
        name <- getMandatoryString(newTheoremDefininition.name, "Theorem name")
        premises <- newTheoremDefininition.premises.flatMap(getOptionalString).mapWithIndex((str, index) => Statement.parser.parseFromString(str, s"premise ${index + 1}").recoverWithBadRequest).traverseTry
        conclusion <- Statement.parser.parseFromString(newTheoremDefininition.conclusion, "conclusion").recoverWithBadRequest
        newTheorem = Theorem(
          name,
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
      implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
      val name = getOptionalString(newStatementDefinition.name)
      val shorthand = getOptionalString(newStatementDefinition.shorthand)
      val attributes = getWords(newStatementDefinition.attributes)
      for {
        symbol <- getMandatoryString(newStatementDefinition.symbol, "Symbol")
        boundVariablesAndComponentTypes <- ExpressionDefinition.rawBoundVariablesAndComponentTypesParser.parseFromString(newStatementDefinition.components, "components").recoverWithBadRequest
        boundVariables = boundVariablesAndComponentTypes._1
        componentTypes = boundVariablesAndComponentTypes._2
        componentNames = boundVariables ++ componentTypes.map(_.name)
        definition <- newStatementDefinition.definition.filter(_.nonEmpty).map(d => Statement.parser(expressionParsingContext.addInitialParameter("_")).parseFromString(d, "definition").recoverWithBadRequest).swap
        format <- getFormat(newStatementDefinition.format, symbol, componentNames)
        newStatementDefinition = StatementDefinition(
          symbol,
          boundVariables,
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
      implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
      val name = getOptionalString(newTermDefinition.name)
      val shorthand = getOptionalString(newTermDefinition.shorthand)
      val attributes = getWords(newTermDefinition.attributes)
      for {
        symbol <- getMandatoryString(newTermDefinition.symbol, "Symbol")
        disambiguator <- getOptionalSingleWord(newTermDefinition.disambiguator, "Disambiguator")
        boundVariablesAndComponentTypes <- ExpressionDefinition.rawBoundVariablesAndComponentTypesParser.parseFromString(newTermDefinition.components, "components").recoverWithBadRequest
        boundVariables = boundVariablesAndComponentTypes._1
        componentTypes = boundVariablesAndComponentTypes._2
        componentNames = boundVariables ++ componentTypes.map(_.name)
        definition <- Statement.parser(expressionParsingContext.addInitialParameter("_")).parseFromString(newTermDefinition.definition, "definition").recoverWithBadRequest
        format <- getFormat(newTermDefinition.format, symbol, componentNames)
        premises <- newTermDefinition.premises.mapWithIndex((str, index) => Statement.parser.parseFromString(str, s"premise ${index + 1}")).recoverWithBadRequest
        newTerm = TermDefinition(
          symbol,
          boundVariables,
          componentTypes,
          disambiguator,
          name,
          format,
          premises,
          definition,
          shorthand,
          attributes)
      } yield newTerm
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/typeDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createTypeDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newTypeDefininition: NewTypeDefinitionModel
  ): ResponseEntity[_] = {
    bookService.addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
      val name = getOptionalString(newTypeDefininition.name)
      for {
        symbol <- getMandatoryString(newTypeDefininition.symbol, "Symbol")
        defaultTermName <- getMandatoryString(newTypeDefininition.defaultTermName, "Default term name")
        otherComponentTypes <- ComponentType.listWithoutBoundVariablesParser.parseFromString(newTypeDefininition.otherComponents, "component types").recoverWithBadRequest
        format <- Format.parser(otherComponentTypes.map(_.name)).parseFromString(newTypeDefininition.format, "format").recoverWithBadRequest
        definition <- Statement.parser.parseFromString(newTypeDefininition.definition, "definition").recoverWithBadRequest
        newTypeDefinition = TypeDefinition(
          symbol,
          defaultTermName,
          otherComponentTypes,
          format,
          name,
          definition)
      } yield newTypeDefinition
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/propertyDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createPropertyDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newPropertyDefininition: NewPropertyDefinitionModel
  ): ResponseEntity[_] = {
    bookService.addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
      val name = getOptionalString(newPropertyDefininition.name)
      for {
        symbol <- getMandatoryString(newPropertyDefininition.symbol, "Symbol")
        defaultTermName <- getMandatoryString(newPropertyDefininition.defaultTermName, "Default term name")
        parentType <- entryContext.typeDefinitions.find(_.symbol == newPropertyDefininition.parentType).orBadRequest(s"Unknown type '${newPropertyDefininition.parentType}'")
        parentComponentTypes <- parentType.childComponentTypesParser.parseFromString(newPropertyDefininition.parentComponents, "parent component types").recoverWithBadRequest
        definition <- Statement.parser.parseFromString(newPropertyDefininition.definition, "definition").recoverWithBadRequest
        newPropertyDefinition = PropertyDefinitionOnType(
          symbol,
          parentType,
          defaultTermName,
          parentComponentTypes,
          name,
          definition)
      } yield newPropertyDefinition
    }.map{ case (books, definitions, book, chapter) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  @PostMapping(value = Array("/standalonePropertyDefinitions"), produces = Array("application/json;charset=UTF-8"))
  def createStandalonePropertyDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newPropertyDefininition: NewStandalonePropertyDefinitionModel
  ): ResponseEntity[_] = {
    bookService.addChapterEntry(bookKey, chapterKey) { (books, book, chapter) =>
      implicit val entryContext: EntryContext = EntryContext.forChapterInclusive(books, book, chapter)
      implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
      val name = getOptionalString(newPropertyDefininition.name)
      for {
        symbol <- getMandatoryString(newPropertyDefininition.symbol, "Symbol")
        defaultTermName <- getMandatoryString(newPropertyDefininition.defaultTermName, "Default term name")
        otherComponentTypes <- ComponentType.listWithoutBoundVariablesParser.parseFromString(newPropertyDefininition.otherComponents, "component types").recoverWithBadRequest
        format <- Format.parser(otherComponentTypes.map(_.name)).parseFromString(newPropertyDefininition.format, "format").recoverWithBadRequest
        definition <- Statement.parser.parseFromString(newPropertyDefininition.definition, "definition").recoverWithBadRequest
        newPropertyDefinition = StandalonePropertyDefinition(
          symbol,
          defaultTermName,
          otherComponentTypes,
          format,
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

  @PutMapping(value = Array("/{entryKey}/shorthand"), produces = Array("application/json;charset=UTF-8"))
  def editShorthand(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newShorthand: String
  ): ResponseEntity[_] = {
    bookService.modifyEntry[ExpressionDefinition, Identity](bookKey, chapterKey, entryKey, (_, _, _, _, definition) =>
      Success(definition.withShorthand(Option(newShorthand).filter(_.nonEmpty)))
    ).map{ case (books, definitions, book, chapter, _) => getChapterProps(books, definitions, book, bookKey, chapter, chapterKey) }.toResponseEntity
  }

  private def getDefinitionShorthands(entryContext: EntryContext): Map[String, DisambiguatedSymbol] = {
    val shorthandsFromDefinitions = entryContext.availableEntries.ofType[ExpressionDefinition].mapCollect(d => d.shorthand.map(_ -> d.disambiguatedSymbol)).toMap
    val greekLetterShorthands = 'α'.to('ω')
      .map(c => Character.getName(c).splitByWhitespace().last.toLowerCase -> DisambiguatedSymbol(c.toString, None))
      .toMap
    shorthandsFromDefinitions ++ greekLetterShorthands
  }

  case class BinaryStatementSummary(symbol: String, template: Statement, attributes: Seq[String], isTransitive: Boolean)
  private def getBinaryRelations(provingContext: ProvingContext): Seq[BinaryStatementSummary] = {
    provingContext.definedBinaryJoiners.map { relation =>
      BinaryStatementSummary(relation.symbol, relation.template, relation.attributes, provingContext.transitivities.exists(_.isTransitivityForJoiner(relation)))
    }
  }

}

object ChapterController {
  case class NewTheoremModel(
    name: String,
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
    otherComponents: String,
    format: String,
    name: String,
    definition: String)
  case class NewPropertyDefinitionModel(
    symbol: String,
    parentType: String,
    defaultTermName: String,
    parentComponents: String,
    name: String,
    definition: String)
  case class NewStandalonePropertyDefinitionModel(
    symbol: String,
    defaultTermName: String,
    otherComponents: String,
    format: String,
    name: String,
    definition: String)
}
