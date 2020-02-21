package net.prover.controllers

import net.prover.exceptions.BadRequestException
import net.prover.model.entries.{ChapterEntry, ExpressionDefinition}
import net.prover.model.{Book, EntryContext, Format, Inference}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Failure, Success}

@RestController
@RequestMapping(Array("/books/{bookKey}/{chapterKey}/{entryKey}"))
class EntryController @Autowired() (val bookService: BookService) extends BookModification {

  @PutMapping(value = Array("/name"), produces = Array("application/json;charset=UTF-8"))
  def editName(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newName: String
  ): ResponseEntity[_] = {
    bookService.modifyEntry[ChapterEntry, Identity](bookKey, chapterKey, entryKey, (_, _, _, _, entry) => entry match {
      case inference: Inference.Entry =>
        Success(inference.withName(newName))
      case definition: ExpressionDefinition =>
        Success(definition.withName(Option(newName).filter(_.nonEmpty)))
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
      entry <- bookService.findEntry[ExpressionDefinition](chapter, entryKey)
      newEntry = entry.withSymbol(newSymbol)
      newEntryWithFormat = if (entry.format.isInstanceOf[Format.Default]) newEntry.withFormat(Format.default(newSymbol, entry.boundVariableNames ++ entry.componentTypes.map(_.name))) else newEntry
      _ = modifyDefinition(entry, newEntryWithFormat)
    } yield ()).toResponseEntity
  }

  @PutMapping(value = Array("/attributes"), produces = Array("application/json;charset=UTF-8"))
  def editAttributes(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newAttributes: Seq[String]
  ): ResponseEntity[_] = {
    (for {
      book <- bookService.findBook(bookKey)
      chapter <- bookService.findChapter(book, chapterKey)
      entry <- bookService.findEntry[ExpressionDefinition](chapter, entryKey)
      newEntry = entry.withAttributes(newAttributes)
      _ = modifyDefinition(entry, newEntry)
    } yield ()).toResponseEntity
  }

  @PutMapping(value = Array("/format"), produces = Array("application/json;charset=UTF-8"))
  def editFormat(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody(required = false) newFormatText: String
  ): ResponseEntity[_] = {
    (for {
      book <- bookService.findBook(bookKey)
      chapter <- bookService.findChapter(book, chapterKey)
      entry <- bookService.findEntry[ExpressionDefinition](chapter, entryKey)
      componentNames = entry.boundVariableNames ++ entry.componentTypes.map(_.name)
      format <- Format.parser(componentNames).parseFromString(newFormatText, "format").recoverWithBadRequest
      newEntry = entry.withFormat(format)
      _ = modifyDefinition(entry, newEntry)
    } yield ()).toResponseEntity
  }

  private def modifyDefinition(oldDefinition: ExpressionDefinition, newDefinition: ExpressionDefinition): Unit = {
    bookService.modifyBooks[Identity]((books, _) => {
      books.mapReduceWithPrevious[Book] { (previousBooks, bookToModify) =>
        bookToModify.chapters.mapFold(EntryContext.forBookExclusive(previousBooks, bookToModify)) { (entryContextForChapter, chapterToModify) =>
          chapterToModify.entries.mapFold(entryContextForChapter) { (entryContext, entryToModify) =>
            val modifiedEntry = if (entryToModify == oldDefinition) {
              newDefinition
            } else {
              entryToModify.replaceDefinition(oldDefinition, newDefinition, entryContext)
            }
            (entryContext.addEntry(modifiedEntry), modifiedEntry)
          }.mapRight(newEntries => chapterToModify.copy(entries = newEntries))
        }.mapRight(newChapters => bookToModify.copy(chapters = newChapters))._2
      }
    })
  }
}
