package net.prover.controllers

import net.prover.controllers.models.{DefinitionSummary, LinkSummary, StandalonePropertyDefinitionSummary, TypeDefinitionSummary}
import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition
import net.prover.model.entries._
import net.prover.model.expressions.Statement
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Failure, Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{entryKey}"))
class EntryController @Autowired() (val bookService: BookService) extends BookModification with ParameterValidation with ReactViews {

  @GetMapping(value = Array(""), produces = Array("text/html;charset=UTF-8"))
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
        case statementDefinition: StatementDefinitionEntry =>
          Success(("StatementDefinition", Map("definition" -> statementDefinition)))
        case termDefinition: TermDefinitionEntry =>
          Success(("TermDefinition", Map("definition" -> termDefinition)))
        case typeDefinition: TypeDefinition =>
          Success(("TypeDefinition", Map("definition" -> typeDefinition)))
        case typeQualifierDefinition: TypeQualifierDefinition =>
          Success(("TypeQualifierDefinition", Map("definition" -> typeQualifierDefinition)))
        case propertyDefinitionOnType: PropertyDefinitionOnType =>
          Success(("PropertyDefinitionOnType", Map("definition" -> propertyDefinitionOnType)))
        case relatedObjectDefinition: RelatedObjectDefinition =>
          Success(("RelatedObjectDefinition", Map("definition" -> relatedObjectDefinition)))
        case standalonePropertyDefinition: StandalonePropertyDefinition =>
          Success(("StandalonePropertyDefinition", Map("definition" -> standalonePropertyDefinition)))
        case _ =>
          Failure(BadRequestException(s"Cannot view ${entry.getClass.getSimpleName}"))
      }
    } yield {
      val provingContext = ProvingContext(entryContext, definitions)
      val entriesWithKeys = BookService.getEntriesWithKeys(chapter).mapCollect(_.optionMapLeft(_.asOptionalInstanceOf[ChapterEntry.Standalone]))
      val index = entriesWithKeys.findIndexWhere(_._1 == entry).getOrElse(throw new Exception("Entry somehow didn't exist"))
      val previous = entriesWithKeys.lift(index - 1).map { case (c, key) => LinkSummary(c.title, key) }
      val next = entriesWithKeys.lift(index + 1).map { case (c, key) => LinkSummary(c.title, key) }
      createReactView(viewName, baseProps ++ Map(
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
        "definitionShorthands" -> DefinitionSummary.getDefinitionShorthandsFromContext(entryContext)))
    }).toResponseEntity
  }

  case class BinaryStatementSummary(symbol: String, template: Statement, attributes: Seq[String], isTransitive: Boolean)
  private def getBinaryRelations(provingContext: ProvingContext): Seq[BinaryStatementSummary] = {
    provingContext.definedBinaryJoiners.map { relation =>
      BinaryStatementSummary(relation.symbol, relation.template, relation.attributes, provingContext.transitivities.exists(_.isTransitivityForJoiner(relation)))
    }
  }

  @PutMapping(value = Array("/name"), produces = Array("application/json;charset=UTF-8"))
  def editName(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newName: String
  ): ResponseEntity[_] = {
    modifyEntryWithReplacement(bookKey, chapterKey, entryKey) {
      case (entry: ChapterEntry.HasChangeableName, _) =>
        getMandatoryString(newName, "name").map(entry.withName)
      case (entry: ChapterEntry.HasOptionalExplicitName, _) =>
        Success(entry.withName(getOptionalString(newName)))
      case (entry, _) =>
        Failure(BadRequestException(s"Cannot set name of ${entry.getClass.getName}"))
    }
  }

  @PutMapping(value = Array("/symbol"), produces = Array("application/json;charset=UTF-8"))
  def editSymbol(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newSymbol: String
  ): ResponseEntity[_] = {
    modifyEntryWithReplacement(bookKey, chapterKey, entryKey) {
      case (entry: ChapterEntry.HasSymbol, _) =>
        Success(entry.withSymbol(newSymbol))
      case (entry, _) =>
        Failure(BadRequestException(s"Cannot edit symbol of ${entry.getClass.getName}"))
    }
  }

  @PutMapping(value = Array("/disambiguator"), produces = Array("application/json;charset=UTF-8"))
  def editDisambiguator(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newDisambiguator: String
  ): ResponseEntity[_] = {
    modifyEntryWithReplacement(bookKey, chapterKey, entryKey) {
      case (definition: TermDefinitionEntry, _) =>
        for {
          disambiguator <- getOptionalSingleWord(newDisambiguator, "Disambiguator")
        } yield definition.withDisambiguator(disambiguator)
      case (entry, _) =>
        Failure(BadRequestException(s"Cannot edit symbol of ${entry.getClass.getName}"))
    }
  }

  @PutMapping(value = Array("/disambiguatorAdders"), produces = Array("application/json;charset=UTF-8"))
  def editDisambiguatorAdders(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) serializedNewDisambiguatorAdders: Seq[String]
  ): ResponseEntity[_] = {
    modifyEntryWithReplacement(bookKey, chapterKey, entryKey) {
      case (definition: TermDefinitionEntry, entryContext) =>
        for {
          newDisambiguatorAdders <- serializedNewDisambiguatorAdders.mapWithIndex((s, i) => DisambiguatorAdder.parser(entryContext).parseFromString(s, s"disambiguator adder ${i + 1}").recoverWithBadRequest).traverseTry
        } yield definition.withDisambiguatorAdders(newDisambiguatorAdders)
      case (entry, _) =>
        Failure(BadRequestException(s"Cannot set disambiguator adders of ${entry.getClass.getName}"))
    }
  }

  @PutMapping(value = Array("/attributes"), produces = Array("application/json;charset=UTF-8"))
  def editAttributes(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newAttributes: Seq[String]
  ): ResponseEntity[_] = {
    modifyEntryWithReplacement(bookKey, chapterKey, entryKey) {
      case (definition: ExpressionDefinitionEntry, _) =>
        Success(definition.withAttributes(newAttributes))
      case (entry, _) =>
        Failure(BadRequestException(s"Cannot set attributes of ${entry.getClass.getName}"))
    }
  }

  @PutMapping(value = Array("/shorthand"), produces = Array("application/json;charset=UTF-8"))
  def editShorthand(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newShorthand: String
  ): ResponseEntity[_] = {
    modifyEntryWithReplacement(bookKey, chapterKey, entryKey) {
      case (definition: ExpressionDefinitionEntry, _) =>
        Success(definition.withShorthand(getOptionalString(newShorthand)))
      case (entry, _) =>
        Failure(BadRequestException(s"Cannot set shorthand of ${entry.getClass.getName}"))
    }
  }

  @PutMapping(value = Array("/format"), produces = Array("application/json;charset=UTF-8"))
  def editFormat(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) rawNewFormatText: String
  ): ResponseEntity[_] = {
    modifyEntryWithReplacement(bookKey, chapterKey, entryKey) {
      case (definition: ExpressionDefinitionEntry, _) =>
        val format = getOptionalString(rawNewFormatText) match {
          case Some(newFormatText) =>
            Format.parserForExpressionDefinition(definition.baseSymbol, definition.boundVariableNames, definition.componentTypes).parseFromString(newFormatText, "format").recoverWithBadRequest
          case None =>
            Format.default(definition.boundVariableNames, definition.componentTypes).recoverWithBadRequest
        }
        format.map(definition.withFormat)
      case (definition: TypeDefinition, _) =>
        for {
          newFormatText <- getMandatoryString(rawNewFormatText, "format")
          qualifier <- definition.defaultQualifier.orBadRequest("Cannot update format on type with no qualifier")
          format <- Format.parserForTypeDefinition(qualifier.variableDefinitions).parseFromString(newFormatText, "format").recoverWithBadRequest
        } yield definition.withFormat(format)
      case (entry, _) =>
        Failure(BadRequestException(s"Cannot set attributes of ${entry.getClass.getName}"))
    }
  }

  @PutMapping(value = Array("/mainVariableDefinition"), produces = Array("application/json;charset=UTF-8"))
  def editMainVariableDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newMainVariableDefinitionText: String
  ): ResponseEntity[_] = {
    modifyEntryWithReplacement(bookKey, chapterKey, entryKey) {
      case (entry: ChapterEntry.HasMainVariable, _) =>
        getSimpleVariableDefinition(newMainVariableDefinitionText, "main variable definition").map(entry.withMainVariableDefinition)
      case (entry, _) =>
        Failure(BadRequestException(s"Cannot set main term variable of ${entry.getClass.getName}"))
    }
  }

  private def modifyEntryWithReplacement(oldEntry: ChapterEntry, newEntry: ChapterEntry): Seq[Book] = {
    bookService.modifyBooks[Identity]((books, _) => {
      books.mapFoldWithPrevious[(Map[ChapterEntry, ChapterEntry], Map[ExpressionDefinition, ExpressionDefinition]), Book]((Map.empty, Map.empty)) { case ((chapterEntries, expressionDefinitions), previousBooks, bookToModify) =>
        bookToModify.chapters.mapFold((EntryContext.forBookExclusive(previousBooks, bookToModify), chapterEntries, expressionDefinitions)) { case ((entryContextForChapter, chapterEntries, expressionDefinitions), chapterToModify) =>
          chapterToModify.entries.mapFold((entryContextForChapter, chapterEntries, expressionDefinitions)) { case ((entryContext, chapterEntries, expressionDefinitions), entryToModify) =>
            val modifiedEntry = if (entryToModify == oldEntry) newEntry else entryToModify.replaceDefinitions(chapterEntries, expressionDefinitions, entryContext)
            val newEntryContext = entryContext.addEntry(modifiedEntry)
            val newChapterEntries = chapterEntries + (entryToModify -> modifiedEntry)
            val newExpressionDefinitions = EntryContext.getStatementDefinitionFromEntry(entryToModify) match {
              case Some(old) =>
                expressionDefinitions + (old -> EntryContext.getStatementDefinitionFromEntry(modifiedEntry).get)
              case None =>
                entryToModify.asOptionalInstanceOf[TermDefinitionEntry] match {
                  case Some(old) =>
                    expressionDefinitions + (old -> modifiedEntry.asInstanceOf[TermDefinitionEntry])
                  case None =>
                    expressionDefinitions
                }
            }
            ((newEntryContext, newChapterEntries, newExpressionDefinitions), modifiedEntry)
          }.mapRight(newEntries => chapterToModify.copy(entries = newEntries))
        }.mapRight(newChapters => bookToModify.copy(chapters = newChapters)).mapLeft { case (_, a, b) => (a, b)}
      }._2
    })._1
  }

  private def modifyEntryWithReplacement(bookKey: String, chapterKey: String, entryKey: String)(f: (ChapterEntry, EntryContext) => Try[ChapterEntry]): ResponseEntity[_] = {
    val books = bookService.books
    (for {
      book <- bookService.findBook(books, bookKey)
      chapter <- bookService.findChapter(book, chapterKey)
      oldEntry <- bookService.findEntry[ChapterEntry](chapter, entryKey)
      entryContext = EntryContext.forEntry(books, book, chapter, oldEntry)
      newEntry <- f(oldEntry, entryContext)
      newBooks = modifyEntryWithReplacement(oldEntry, newEntry)
      newBook <- bookService.findBook(newBooks, bookKey)
      newChapter <- bookService.findChapter(newBook, chapterKey)
      newKey <- BookService.getEntriesWithKeys(newChapter).find(_._1 == newEntry).map(_._2).orException(new Exception("Couldn't find new entry"))
      props = Map("entry" -> newEntry, "url" -> BookService.getEntryUrl(bookKey, chapterKey, newKey))
    } yield props).toResponseEntity
  }
}
