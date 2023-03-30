package net.prover.controllers

import net.prover.books.keys.ListWithKeys
import net.prover.books.model.Book
import net.prover.controllers.models.LinkSummary
import net.prover.entries.{BookWithContext, GlobalContext}
import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition
import net.prover.model.entries._
import net.prover.model.expressions.Statement
import net.prover.util.FunctorTypes._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Failure, Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{entryKey}"))
class EntryController @Autowired() (val bookService: BookService) extends UsageFinder with ParameterValidation with ReactViews {

  @GetMapping(value = Array(""), produces = Array("text/html;charset=UTF-8"))
  def getEntry(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String
  ): ResponseEntity[_] = {
    (for {
      entryWithContext <- bookService.findEntry[ChapterEntry](bookKey, chapterKey, entryKey)
      (viewName, baseProps) <- entryWithContext.entry match {
        case axiom: Axiom =>
          Success(("Axiom", Map("axiom" -> axiom)))
        case theorem: Theorem =>
          Success(("Theorem", Map("theorem" -> theorem, "inferences" -> BookService.getInferenceLinks(theorem.referencedInferenceIds, entryWithContext.globalContext))))
        case statementDefinition: StatementDefinitionEntry =>
          Success(("StatementDefinition", Map("definition" -> statementDefinition)))
        case termDefinition: TermDefinitionEntry =>
          Success(("TermDefinition", Map("definition" -> termDefinition)))
        case typeDefinition: TypeDefinition =>
          Success(("TypeDefinition", Map("definition" -> typeDefinition)))
        case typeQualifierDefinition: TypeQualifierDefinition =>
          Success(("TypeQualifierDefinition", Map("definition" -> typeQualifierDefinition)))
        case typeRelationDefinition: TypeRelationDefinition =>
          Success(("TypeRelationDefinition", Map("definition" -> typeRelationDefinition)))
        case propertyDefinitionOnType: PropertyDefinitionOnType =>
          Success(("PropertyDefinitionOnType", Map("definition" -> propertyDefinitionOnType)))
        case relatedObjectDefinition: RelatedObjectDefinition =>
          Success(("RelatedObjectDefinition", Map("definition" -> relatedObjectDefinition)))
        case standalonePropertyDefinition: StandalonePropertyDefinition =>
          Success(("StandalonePropertyDefinition", Map("definition" -> standalonePropertyDefinition)))
        case _ =>
          Failure(BadRequestException(s"Cannot view ${entryWithContext.entry.getClass.getSimpleName}"))
      }
    } yield {
      import entryWithContext._
      val entriesWithKeys = chapterWithContext.chapter.entriesWithKeys.listWithKeys.mapCollect(_.optionMapLeft(_.asOptionalInstanceOf[ChapterEntry.Standalone]))
      val index = entriesWithKeys.findIndexWhere(_._1 == entry).getOrElse(throw new Exception("Entry somehow didn't exist"))
      val previous = entriesWithKeys.lift(index - 1).map { case (c, key) => LinkSummary(c.title, key) }
      val next = entriesWithKeys.lift(index + 1).map { case (c, key) => LinkSummary(c.title, key) }
      createReactView(viewName, baseProps ++ Map(
        "url" -> BookService.getEntryUrl(entryWithContext),
        "bookLink" -> LinkSummary(book.title, BookService.getBookUrl(chapterWithContext.bookWithContext)),
        "chapterLink" -> LinkSummary(chapter.title, BookService.getChapterUrl(chapterWithContext)),
        "previous" -> previous,
        "next" -> next,
        "usages" -> getInferenceUsages(entry),
        "binaryRelations" -> getBinaryRelations(provingContext)) ++
        getGeneralDisplayProps(entryContext))
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

  @PutMapping(value = Array("/firstVariable"), produces = Array("application/json;charset=UTF-8"))
  def editFirstVariableDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newVariableDefinitionText: String
  ): ResponseEntity[_] = {
    modifyEntryWithReplacement(bookKey, chapterKey, entryKey) {
      case (entry: TypeRelationDefinition, _) =>
        getSimpleVariableDefinition(newVariableDefinitionText, "main variable definition").map(d => entry.copy(firstVariable = d))
      case (entry, _) =>
        Failure(BadRequestException(s"Cannot set main term variable of ${entry.getClass.getName}"))
    }
  }

  @PutMapping(value = Array("/secondVariable"), produces = Array("application/json;charset=UTF-8"))
  def editSecondVariableDefinition(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newVariableDefinitionText: String
  ): ResponseEntity[_] = {
    modifyEntryWithReplacement(bookKey, chapterKey, entryKey) {
      case (entry: TypeRelationDefinition, _) =>
        getSimpleVariableDefinition(newVariableDefinitionText, "main variable definition").map(d => entry.copy(secondVariable = d))
      case (entry, _) =>
        Failure(BadRequestException(s"Cannot set main term variable of ${entry.getClass.getName}"))
    }
  }

  @PutMapping(value = Array("/definingStatement"), produces = Array("application/json;charset=UTF-8"))
  def editDefiningStatement(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newDefiningStatementText: String
  ): ResponseEntity[_] = {
    modifyEntryWithReplacement(bookKey, chapterKey, entryKey) {
      case (entry: ChapterEntry.HasDefiningStatement, entryContext) =>
        for {
          _ <- bookService.books.flatMap(_.chapters).flatMap(_.entries)
            .find(_.referencedInferenceIds.exists(entry.inferences.map(_.id).contains))
            .badRequestIfDefined(entryUsing => "Cannot set defining statement - is already depended on by " + entryUsing.name)
          expressionParsingContext = entry.definingStatementParsingContext(entryContext)
          newDefiningStatement <- Statement.parser(expressionParsingContext).parseFromString(newDefiningStatementText, "new defining statement").recoverWithBadRequest
        } yield entry.withDefiningStatement(newDefiningStatement)
      case (entry, _) =>
        Failure(BadRequestException(s"Cannot set defining statement of ${entry.getClass.getName}"))
    }
  }
  case class Changes(changedEntries: Map[ChapterEntry, ChapterEntry], changedDefinitions: Map[ExpressionDefinition, ExpressionDefinition]) {
    def addChangedEntry(oldEntry: ChapterEntry, newEntry: ChapterEntry): Changes = {
      copy(changedEntries = changedEntries + (oldEntry -> newEntry))
    }
    def addChangedDefinition(oldDefinition: ExpressionDefinition, newDefinition: ExpressionDefinition): Changes = {
      copy(changedDefinitions = changedDefinitions + (oldDefinition -> newDefinition))
    }
  }

  private def replaceEntryInBooks(globalContext: GlobalContext, oldEntry: ChapterEntry, newEntry: ChapterEntry): Seq[Book] = {
    globalContext.booksWithContexts.mapFoldWithPrevious[Changes, Book](Changes(Map.empty, Map.empty)) { case (changes, previousBooks, bookToModify) =>
      bookToModify.chaptersWithContexts.mapFoldWithPrevious[Changes, Chapter](changes) { case (changes, previousChapters, chapterToModify) =>
        chapterToModify.entriesWithContexts.mapFoldWithPrevious[Changes, ChapterEntry](changes) { case (changes, previousEntries, entryContextToModify) =>
          val entryToModify = entryContextToModify.entry
          val updatedEntryWithContext = entryContextToModify.copy(
            chapterWithContext = chapterToModify.copy(
              chapter = chapterToModify.chapter.setEntries(previousEntries.toList),
              bookWithContext = bookToModify.copy(
                book = bookToModify.book.setChapters(previousChapters.toList),
                globalContext = globalContext.copy(booksWithKeys = ListWithKeys(previousBooks.toList)))))
          val modifiedEntry = if (entryToModify == oldEntry) newEntry else entryToModify.replaceDefinitions(changes.changedEntries, changes.changedDefinitions, updatedEntryWithContext)
          val changesWithEntry = changes.addChangedEntry(entryToModify, newEntry)
          val changesWithExpressionDefinitions = EntryContext.getStatementDefinitionFromEntry(entryToModify) match {
            case Some(old) =>
              changesWithEntry.addChangedDefinition(old, EntryContext.getStatementDefinitionFromEntry(modifiedEntry).get)
            case None =>
              entryToModify.asOptionalInstanceOf[TermDefinitionEntry] match {
                case Some(old) =>
                  changesWithEntry.addChangedDefinition(old, modifiedEntry.asInstanceOf[TermDefinitionEntry])
                case None =>
                  changesWithEntry
              }
          }
          (changesWithExpressionDefinitions, modifiedEntry)
        }.mapRight(newEntries => chapterToModify.chapter.setEntries(newEntries.toList))
      }.mapRight(newChapters => bookToModify.book.setChapters(newChapters.toList))
    }._2
  }

  private def modifyEntryWithReplacement(bookKey: String, chapterKey: String, entryKey: String)(f: (ChapterEntry, EntryContext) => Try[ChapterEntry]): ResponseEntity[_] = {
    bookService.modifyBooks[TryWithValue[ChapterEntry]#Type](globalContext => {
      for {
        oldEntryWithContext <- globalContext.findEntry(bookKey, chapterKey, entryKey)
        newEntry <- f(oldEntryWithContext.entry, oldEntryWithContext.entryContext)
        newBooks = replaceEntryInBooks(globalContext, oldEntryWithContext.entry, newEntry)
      } yield (newBooks, newEntry)
    }).flatMap { case (globalContext, newEntry) =>
      for {
        chapterWithContext <- globalContext.findChapter(bookKey, chapterKey)
        newKey <- chapterWithContext.chapter.entriesWithKeys.listWithKeys.find(_._1 == newEntry).map(_._2).orException(new Exception("Couldn't find new entry"))
        props = Map("entry" -> newEntry, "url" -> BookService.getEntryUrl(bookKey, chapterKey, newKey))
      } yield props
    }.toResponseEntity
  }
}
