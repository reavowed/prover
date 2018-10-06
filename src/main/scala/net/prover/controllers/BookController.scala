package net.prover.controllers

import monocle.macros.GenLens
import net.prover.model.entries._
import net.prover.model.{Book, Chapter, Inference}
import net.prover.services.BookService
import net.prover.views._
import org.slf4j.LoggerFactory
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.{HttpStatus, ResponseEntity}
import org.springframework.web.bind.annotation._
import net.prover.utils.Traversals

import scala.util.control.NonFatal

@RestController
@RequestMapping(Array("/books"))
class BookController @Autowired() (bookService: BookService) {
  @GetMapping(value = Array(""), produces = Array("text/html;charset=UTF-8"))
  def get = {
    try {
      BooksView(bookService.books).toString
    } catch {
      case NonFatal(e) =>
        BookController.logger.error("Error getting books", e)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @GetMapping(value = Array("/{bookKey}"), produces = Array("text/html;charset=UTF-8"))
  def getBook(@PathVariable("bookKey") bookKey: String) = {
    try {
      bookService.books.find(_.key.value == bookKey) match {
        case Some(book) =>
          BookView(book).toString
        case None =>
          new ResponseEntity(HttpStatus.NOT_FOUND)
      }
    } catch {
      case NonFatal(e) =>
        BookController.logger.error("Error getting books", e)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @GetMapping(value = Array("/{bookKey}/{chapterKey}"), produces = Array("text/html;charset=UTF-8"))
  def getChapter(@PathVariable("bookKey") bookKey: String, @PathVariable("chapterKey") chapterKey: String) = {
    try {
      (for {
        book <- bookService.books.find(_.key.value == bookKey)
        chapter <- book.chapters.find(_.key.value == chapterKey)
      } yield ChapterView(chapter, book).toString) getOrElse new ResponseEntity(HttpStatus.NOT_FOUND)
    } catch {
      case NonFatal(e) =>
        BookController.logger.error("Error getting books", e)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @GetMapping(value = Array("/{bookKey}/{chapterKey}/{entryKey}"), produces = Array("text/html;charset=UTF-8"))
  def getEntry(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String
  ) = {
    try {
      val books = bookService.books
      (for {
        book <- books.find(_.key.value == bookKey)
        chapter <- book.chapters.find(_.key.value == chapterKey)
        entries = chapter.entries.ofType[ChapterEntry.Standalone]
        entry <- entries.find(_.key.value == entryKey)
      } yield {
        val index = entries.indexOf(entry)
        val previous = if (index > 0) Some(entries(index - 1)) else None
        val next = if (index < entries.length - 1) Some(entries(index + 1)) else None
        entry match {
          case axiom: Axiom =>
            AxiomView(axiom, chapter, book, previous, next, getUsages(axiom, books)).toString
          case theorem: Theorem =>
            TheoremView(theorem, chapter, book, previous, next, getUsages(theorem, books)).toString
        }
      }) getOrElse new ResponseEntity(HttpStatus.NOT_FOUND)
    } catch {
      case NonFatal(e) =>
        BookController.logger.error(s"Error getting books", e)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @PutMapping(value = Array("/{bookKey}/{chapterKey}/{entryKey}/shorthand"), produces = Array("text/html;charset=UTF-8"))
  def editShorthand(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("entryKey") entryKey: String,
    @RequestBody newShorthand: String
  ) = {
    bookService.updateBooks(Traversals.filter[Book](_.key.value == bookKey)
      .composeLens(GenLens[Book](_.chapters))
      .composeTraversal(Traversals.filter[Chapter](_.key.value == chapterKey))
      .composeLens(GenLens[Chapter](_.entries))
      .composeTraversal(Traversals.filterWithType[ChapterEntry, ExpressionDefinition](_.key.value == entryKey))
      .modify(_.withShorthand(Option(newShorthand).filter(_.nonEmpty))))
  }

  private def getUsages(inference: Inference, books: Seq[Book]): Seq[(Book, Chapter, Seq[Theorem])] = {
    for {
      book <- books
      chapter <- book.chapters
      theorems = chapter.entries.ofType[Theorem].filter(_.proof.referencedInferenceIds.contains(inference.id))
      if theorems.nonEmpty
    } yield (book, chapter, theorems)
  }
}

object BookController {
  val logger = LoggerFactory.getLogger(BookController.getClass)
}
