package net.prover.controllers

import net.prover.books.model.Book
import net.prover.controllers.BookController.ChapterDefinition
import net.prover.controllers.models.LinkSummary
import net.prover.model._
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.{Success, Try}

@RestController
@RequestMapping(Array("/books/{bookKey}"))
class BookController @Autowired() (val bookService: BookService) extends BookModification with ReactViews {

  case class ChapterSummary(title: String, url: String, summary: String)
  case class BookProps(title: String, url: String, chapters: Seq[ChapterSummary], previous: Option[LinkSummary], next: Option[LinkSummary])
  def createBookProps(book: Book, bookKey: String, booksWithKeys: Seq[(Book, String)]): BookProps = {
    val index = booksWithKeys.findIndexWhere(_._1 == book).getOrElse(throw new Exception("Book somehow didn't exist"))
    val previous = booksWithKeys.lift(index - 1).map { case (b, key) => LinkSummary(b.title, key) }
    val next = booksWithKeys.lift(index + 1).map { case (b, key) => LinkSummary(b.title, key) }
    val chapterSummaries = BookService.getChaptersWithKeys(book).map { case (c, key) => ChapterSummary(c.title, BookService.getChapterUrl(bookKey, key), c.summary) }
    BookProps(book.title, BookService.getBookUrl(bookKey), chapterSummaries, previous, next)
  }

  @GetMapping(produces = Array("text/html;charset=UTF-8"))
  def getBook(@PathVariable("bookKey") bookKey: String): ResponseEntity[_] = {
    val booksWithKeys = bookService.getBooksWithKeys
    (for {
      book <- bookService.findBook(booksWithKeys, bookKey)
    } yield {
      createReactView("Book", createBookProps(book, bookKey, booksWithKeys))
    }).toResponseEntity
  }

  @PostMapping(value = Array("/chapters"), produces = Array("application/json;charset=UTF-8"))
  def createChapter(
    @PathVariable("bookKey") bookKey: String,
    @RequestBody chapterDefinition: ChapterDefinition
  ): ResponseEntity[_] = {
    bookService.modifyBook[Identity](bookKey, (_, _, book) => {
      val chapter = new Chapter(chapterDefinition.title, chapterDefinition.summary, Nil)
      val newBook = book.copy(chapters = book.chapters :+ chapter)
      Success(newBook)
    }).map { case (books, _, book) => createBookProps(book, bookKey, BookService.getBooksWithKeys(books)) }.toResponseEntity
  }

  @DeleteMapping(value = Array("/{chapterKey}"), produces = Array("application/json;charset=UTF-8"))
  def deleteChapter(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String
  ): ResponseEntity[_] = {
    bookService.modifyBook[Identity](bookKey, (books, _, book) => {
      val entriesAfterInThisBook = BookService.getChaptersWithKeys(book).view
        .dropUntil { case (_, key) => key == chapterKey }
        .flatMap(_._1.entries)
      val entriesInOtherBooks = (BookService.getBooksWithKeys(books).filter(_._1 != book)).view
        .dropUntil{ case (_, key) => key == bookKey }
        .flatMap(_._1.chapters)
        .flatMap(_.entries)
      for {
        (chapter, _) <- BookService.getChaptersWithKeys(book).find { case (_, key) => key == chapterKey } orNotFound s"Chapter $chapterKey"
        _ <- findUsage(entriesAfterInThisBook ++ entriesInOtherBooks, chapter.entries).badRequestIfDefined { case (usedEntry, entryUsing) => s"""Entry "${entryUsing.name}" depends on "${usedEntry.name}""""}
      } yield {
        book.copy(chapters = BookService.getChaptersWithKeys(book).filter { case (_, key ) => key != chapterKey }.map(_._1))
      }
    }).map { case (books, _, book) => createBookProps(book, bookKey, BookService.getBooksWithKeys(books)) }.toResponseEntity
  }

  @PutMapping(value = Array("/{chapterKey}/index"), produces = Array("application/json;charset=UTF-8"))
  def moveChapterToIndex(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newIndex: Int
  ): ResponseEntity[_] = {
    def tryMove(chapter: Chapter, previousChapters: Seq[Chapter], nextChapters: Seq[Chapter]): Try[Seq[Chapter]] = {
      previousChapters.takeAndRemainingIfValid(newIndex).map { case (firstChapters, chaptersToMoveBefore) =>
        for {
          _ <- findUsage(chapter.entries, chaptersToMoveBefore.flatMap(_.entries)).badRequestIfDefined { case (entryUsing, usedEntry) => s"""Entry "${entryUsing.name}" depends on "${usedEntry.name}""""}
        } yield (firstChapters :+ chapter) ++ chaptersToMoveBefore ++ nextChapters
      } orElse nextChapters.takeAndRemainingIfValid(newIndex - previousChapters.length).map { case (chaptersToMoveAfter, lastChapters) =>
        for {
          _ <- findUsage(chaptersToMoveAfter.flatMap(_.entries), chapter.entries).badRequestIfDefined { case (entryUsing, usedEntry) => s"""Entry "${entryUsing.name}" depends on "${usedEntry.name}""""}
        } yield (previousChapters ++ chaptersToMoveAfter :+ chapter) ++ lastChapters
      } orBadRequest "Invalid index" flatten
    }
    bookService.modifyBook[Identity](bookKey, (_, _, book) => {
      for {
        (previousChapters, chapter, nextChapters) <- BookService.getChaptersWithKeys(book).splitWhere(_._2 == chapterKey).orNotFound(s"Chapter $chapterKey")
        updatedChapters <- tryMove(chapter._1, previousChapters.map(_._1), nextChapters.map(_._1))
      } yield book.copy(chapters = updatedChapters)
    }).map { case (books, _, book) => createBookProps(book, bookKey, BookService.getBooksWithKeys(books)) }.toResponseEntity
  }
}

object BookController {
  val logger: Logger = LoggerFactory.getLogger(BookController.getClass)

  case class ChapterDefinition(title: String, summary: String)
}
