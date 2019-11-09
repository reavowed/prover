package net.prover.controllers

import net.prover.model.{Book, EntryContext, Inference}
import net.prover.model.entries.{ExpressionDefinition, TermDefinition}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.{PathVariable, PutMapping, RequestBody, RequestMapping, RestController}

import scala.util.Success

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
    modifyEntry[Inference.Entry, Identity](bookKey, chapterKey, entryKey, (_, _, _, inference) => Success(inference.withName(newName)))
      .flatMap { case (_, _, chapter, entry) => {
        for {
          (_, newKey) <- getEntriesWithKeys(chapter).find(_._1 == entry).orException(new Exception("Couldn't find new entry"))
        } yield getEntryUrl(bookKey, chapterKey, newKey)
      }}
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
      book <- findBook(bookKey)
      chapter <- findChapter(book, chapterKey)
      entry <- findEntry[ExpressionDefinition](chapter, entryKey)
      newEntry = entry.withSymbol(newSymbol)
      _ = modifyDefinition(entry, newEntry)
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
      book <- findBook(bookKey)
      chapter <- findChapter(book, chapterKey)
      entry <- findEntry[TermDefinition](chapter, entryKey)
      newEntry = entry.copy(attributes = newAttributes)
      _ = modifyDefinition(entry, newEntry)
    } yield ()).toResponseEntity
  }

  private def modifyDefinition(oldDefinition: ExpressionDefinition, newDefinition: ExpressionDefinition): Unit = {
    bookService.modifyBooks[Identity](books => {
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
