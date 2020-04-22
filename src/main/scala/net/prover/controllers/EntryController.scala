package net.prover.controllers

import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.definitions.{Definitions, ExpressionDefinition}
import net.prover.model.entries.{ChapterEntry, ExpressionDefinitionEntry, TermDefinitionEntry, TypeDefinition}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Failure, Success}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{entryKey}"))
class EntryController @Autowired() (val bookService: BookService) extends BookModification with ParameterValidation {

  @PutMapping(value = Array("/name"), produces = Array("application/json;charset=UTF-8"))
  def editName(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newName: String
  ): ResponseEntity[_] = {
    bookService.modifyEntry[ChapterEntry, Identity](bookKey, chapterKey, entryKey, (_, _, _, _, entry) => entry match {
      case entry: ChapterEntry.CanChangeName =>
        getMandatoryString(newName, "name").map(entry.withName)
      case entry: ChapterEntry.CanChangeOptionalName =>
        Success(entry.withName(getOptionalString(newName)))
      case _ =>
        Failure(BadRequestException(s"Cannot set name of ${entry.getClass.getName}"))
    })
      .flatMap { case (_, _, _, chapter, entry) =>
        for {
          (_, newKey) <- BookService.getEntriesWithKeys(chapter).find(_._1 == entry).orException(new Exception("Couldn't find new entry"))
        } yield BookService.getEntryUrl(bookKey, chapterKey, newKey)
      }
      .toResponseEntity
  }

  @PutMapping(value = Array("/symbol"), produces = Array("application/json;charset=UTF-8"))
  def editSymbol(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newSymbol: String
  ): ResponseEntity[_] = {
    (for {
      book <- bookService.findBook(bookKey)
      chapter <- bookService.findChapter(book, chapterKey)
      entry <- bookService.findEntry[ChapterEntry](chapter, entryKey)
      (newEntry, newBooks) <- entry match {
        case definition: ExpressionDefinitionEntry =>
          val newDefinition = definition.withSymbol(newSymbol)
          Success((newDefinition, modifyEntryWithReplacement(definition, newDefinition)))
        case definition: TypeDefinition =>
          val newDefinition = definition.withSymbol(newSymbol)
          Success((newDefinition, modifyEntryWithReplacement(definition, newDefinition)))
        case _ =>
          Failure(BadRequestException(s"Cannot edit symbol of ${entry.getClass.getName}"))
      }
      newBook <- bookService.findBook(newBooks, bookKey)
      newChapter <- bookService.findChapter(newBook, chapterKey)
      newKey <- BookService.getEntriesWithKeys(newChapter).find(_._1 == newEntry).map(_._2).orException(new Exception("Couldn't find new entry"))
    } yield BookService.getEntryUrl(bookKey, chapterKey, newKey)).toResponseEntity
  }

  @PutMapping(value = Array("/disambiguator"), produces = Array("application/json;charset=UTF-8"))
  def editDisambiguator(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newDisambiguator: String
  ): ResponseEntity[_] = {
    (for {
      book <- bookService.findBook(bookKey)
      chapter <- bookService.findChapter(book, chapterKey)
      entry <- bookService.findEntry[ChapterEntry](chapter, entryKey)
      disambiguator <- getOptionalSingleWord(newDisambiguator, "Disambiguator")
      (newEntry, newBooks) <- entry match {
        case definition: TermDefinitionEntry =>
          val newDefinition = definition.withDisambiguator(disambiguator)
          Success((newDefinition, modifyEntryWithReplacement(definition, newDefinition)))
        case _ =>
          Failure(BadRequestException(s"Cannot edit disambiguator of ${entry.getClass.getName}"))
      }
      newBook <- bookService.findBook(newBooks, bookKey)
      newChapter <- bookService.findChapter(newBook, chapterKey)
      newKey <- BookService.getEntriesWithKeys(newChapter).find(_._1 == newEntry).map(_._2).orException(new Exception("Couldn't find new entry"))
    } yield BookService.getEntryUrl(bookKey, chapterKey, newKey)).toResponseEntity
  }

  @PutMapping(value = Array("/disambiguatorAdders"), produces = Array("application/json;charset=UTF-8"))
  def editDisambiguatorAdders(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) serializedNewDisambiguatorAdders: Seq[String]
  ): ResponseEntity[_] = {
    bookService.modifyEntry[ChapterEntry, Identity](bookKey, chapterKey, entryKey, (allBooks, _, book, chapter, entry) => {
      val entryContext = EntryContext.forEntry(allBooks, book, chapter, entry)
      for {
        newDisambiguatorAdders <- serializedNewDisambiguatorAdders.mapWithIndex((s, i) => DisambiguatorAdder.parser(entryContext).parseFromString(s, s"disambiguator adder ${i + 1}").recoverWithBadRequest).traverseTry
        result <- entry match {
          case definition: TermDefinitionEntry =>
            Success(definition.withDisambiguatorAdders(newDisambiguatorAdders))
          case _ =>
            Failure(BadRequestException(s"Cannot set disambiguator adders of ${entry.getClass.getName}"))
        }
      } yield result
    }).toEmptyResponseEntity
  }

  @PutMapping(value = Array("/attributes"), produces = Array("application/json;charset=UTF-8"))
  def editAttributes(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newAttributes: Seq[String]
  ): ResponseEntity[_] = {
    bookService.modifyEntry[ChapterEntry, Identity](bookKey, chapterKey, entryKey, (_, _, _, _, entry) => entry match {
      case definition: ExpressionDefinitionEntry =>
        Success(definition.withAttributes(newAttributes))
      case _ =>
        Failure(BadRequestException(s"Cannot set attributes of ${entry.getClass.getName}"))
    }).toEmptyResponseEntity
  }

  @PutMapping(value = Array("/format"), produces = Array("application/json;charset=UTF-8"))
  def editFormat(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newFormatText: String
  ): ResponseEntity[_] = {
    bookService.modifyEntry[ChapterEntry, Identity](bookKey, chapterKey, entryKey, (_, _, _, _, entry) => {
      entry match {
        case definition: ExpressionDefinitionEntry =>
          for {
            format <- Format.parserForExpressionDefinition(definition.baseSymbol, definition.boundVariableNames, definition.componentTypes).parseFromString(newFormatText, "format").recoverWithBadRequest
          } yield definition.withFormat(format)
        case definition: TypeDefinition =>
          for {
            qualifier <- definition.qualifier.orBadRequest("Cannot update format on type with no qualifier")
            format <- Format.parser(qualifier.termNames).parseFromString(newFormatText, "format").recoverWithBadRequest
          } yield definition.withFormat(format)
        case _ =>
          Failure(BadRequestException(s"Cannot set format of ${entry.getClass.getName}"))
      }
    }).toEmptyResponseEntity
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
}
