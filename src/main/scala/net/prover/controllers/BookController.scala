package net.prover.controllers

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
    val chapterSummaries = getChaptersWithKeys(book).map { case (c, key) => ChapterSummary(c.title, getChapterUrl(bookKey, key), c.summary) }
    BookProps(book.title, getBookUrl(bookKey), chapterSummaries, previous, next)
  }

  @GetMapping(produces = Array("text/html;charset=UTF-8"))
  def getBook(@PathVariable("bookKey") bookKey: String): ResponseEntity[_] = {
    val booksWithKeys = getBooksWithKeys(bookService.books)
    (for {
      book <- findBook(booksWithKeys, bookKey)
    } yield {
      createReactView("Book", createBookProps(book, bookKey, booksWithKeys))
    }).toResponseEntity
  }

  @PostMapping(value = Array("/chapters"), produces = Array("application/json;charset=UTF-8"))
  def createChapter(
    @PathVariable("bookKey") bookKey: String,
    @RequestBody chapterDefinition: ChapterDefinition
  ): ResponseEntity[_] = {
    modifyBook[Identity](bookKey, (_, _, book) => {
      val chapter = new Chapter(chapterDefinition.title, chapterDefinition.summary, Nil)
      val newBook = book.copy(chapters = book.chapters :+ chapter)
      Success(newBook)
    }).map { case (books, book) => createBookProps(book, bookKey, getBooksWithKeys(books)) }.toResponseEntity
  }

  @DeleteMapping(value = Array("/{chapterKey}"), produces = Array("application/json;charset=UTF-8"))
  def deleteChapter(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String
  ): ResponseEntity[_] = {
    modifyBook[Identity](bookKey, (books, _, book) => {
      val entriesAfterInThisBook = getChaptersWithKeys(book).iterator
        .dropUntil { case (_, key) => key == chapterKey }
        .flatMap(_._1.entries)
      val entriesInLaterBooks = getBooksWithKeys(books).iterator
        .dropUntil{ case (_, key) => key == bookKey }
        .flatMap(_._1.chapters)
        .flatMap(_.entries)
      for {
        (chapter, _) <- getChaptersWithKeys(book).find { case (_, key) => key == chapterKey } orNotFound s"Chapter $chapterKey"
        _ <- !hasUsages(chapter.entries, entriesAfterInThisBook ++ entriesInLaterBooks) orBadRequest "Later chapters / books depend on this one"
      } yield {
        book.copy(chapters = getChaptersWithKeys(book).removeWhere { case (_, key ) => key == chapterKey }.map(_._1))
      }
    }).map { case (books, book) => createBookProps(book, bookKey, getBooksWithKeys(books)) }.toResponseEntity
  }

  @PutMapping(value = Array("/{chapterKey}/index"), produces = Array("application/json;charset=UTF-8"))
  def moveChapterToIndex(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @RequestBody newIndex: Int
  ): ResponseEntity[_] = {
    def tryMove(chapter: Chapter, previousChapters: Seq[Chapter], nextChapters: Seq[Chapter]): Try[Seq[Chapter]] = {
      previousChapters.takeAndRemainingIfValid(newIndex).map { case (firstChapters, chaptersToSkip) =>
        for {
          _ <- (!hasUsages(chapter.entries, chaptersToSkip.flatMap(_.entries))).orBadRequest("Entry depends on a previous one")
        } yield (firstChapters :+ chapter) ++ chaptersToSkip ++ nextChapters
      } orElse nextChapters.takeAndRemainingIfValid(newIndex - previousChapters.length).map { case (chaptersToSkip, lastChapters) =>
        for {
          _ <- (!hasUsages(chaptersToSkip.flatMap(_.entries), chapter.entries)).orBadRequest("Entry depended on by a following one")
        } yield (previousChapters ++ chaptersToSkip :+ chapter) ++ lastChapters
      } orBadRequest "Invalid index" flatten
    }
    modifyBook[Identity](bookKey, (_, _, book) => {
      for {
        (previousChapters, chapter, nextChapters) <- getChaptersWithKeys(book).splitWhere(_._2 == chapterKey).orNotFound(s"Chapter $chapterKey")
        updatedChapters <- tryMove(chapter._1, previousChapters.map(_._1), nextChapters.map(_._1))
      } yield book.copy(chapters = updatedChapters)
    }).map { case (books, book) => createBookProps(book, bookKey, getBooksWithKeys(books)) }.toResponseEntity
  }
}

object BookController {
  val logger: Logger = LoggerFactory.getLogger(BookController.getClass)

  case class ChapterDefinition(title: String, summary: String)
}
