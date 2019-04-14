package net.prover.controllers

import net.prover.controllers.BookController.ChapterDefinition
import net.prover.controllers.models.LinkSummary
import net.prover.model._
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation._

import scala.util.Success

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
    modifyBook[Identity](bookKey, (_, book) => {
      val chapter = new Chapter(chapterDefinition.title, chapterDefinition.summary, Nil)
      val newBook = book.copy(chapters = book.chapters :+ chapter)
      Success(newBook)
    }).map { case (books, book) => createBookProps(book, bookKey, getBooksWithKeys(books)) }.toResponseEntity
  }
}

object BookController {
  val logger: Logger = LoggerFactory.getLogger(BookController.getClass)

  case class ChapterDefinition(title: String, summary: String)
}
