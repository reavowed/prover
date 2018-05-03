package net.prover.controllers

import net.prover.model.entries.{Axiom, Theorem}
import net.prover.model.{Book, Chapter, DisplayContext, Inference}
import net.prover.services.BookService
import org.slf4j.LoggerFactory
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.{HttpStatus, ResponseEntity}
import org.springframework.web.bind.annotation.{GetMapping, PathVariable, RequestMapping, RestController}

import scala.util.control.NonFatal

@RestController
@RequestMapping(Array("/books"))
class BookController @Autowired() (bookService: BookService) {

  @GetMapping(value = Array(""), produces = Array("text/html;charset=UTF-8"))
  def get = {
    try {
      html.books(bookService.books).body
    } catch {
      case NonFatal(e) =>
        BookController.logger.error("Error getting books", e)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @GetMapping(value = Array("/{bookKey}"), produces = Array("text/html;charset=UTF-8"))
  def getBook(@PathVariable("bookKey") bookKey: String) = {
    try {
      bookService.books.find(_.key == bookKey) match {
        case Some(book) =>
          html.book(book).body
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
        book <- bookService.books.find(_.key == bookKey)
        chapter <- book.chapters.find(_.key == chapterKey)
      } yield html.chapter(chapter, book).body) getOrElse new ResponseEntity(HttpStatus.NOT_FOUND)
    } catch {
      case NonFatal(e) =>
        BookController.logger.error("Error getting books", e)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @GetMapping(value = Array("/{bookKey}/{chapterKey}/{inferenceKey}"), produces = Array("text/html;charset=UTF-8"))
  def getInference(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("inferenceKey") inferenceKey: String
  ) = {
    try {
      val books = bookService.books
      (for {
        book <- books.find(_.key == bookKey)
        chapter <- book.chapters.find(_.key == chapterKey)
        inferences = chapter.entries.ofType[Inference.Entry]
        inference <- inferences.find(_.keyOption.contains(inferenceKey))
      } yield {
        val index = inferences.indexOf(inference)
        val previous = if (index > 0) Some(inferences(index - 1)) else None
        val next = if (index < inferences.length - 1) Some(inferences(index + 1)) else None
        implicit val displayContext = DisplayContext((book.dependencies.transitive :+ book).flatMap(_.shorthands))
        inference match {
          case axiom: Axiom =>
            html.axiom(axiom, chapter, book, previous, next, getUsages(axiom, books)).body
          case theorem: Theorem =>
            html.theorem(theorem, chapter, book, previous, next, getUsages(theorem, books)).body
        }
      }) getOrElse new ResponseEntity(HttpStatus.NOT_FOUND)
    } catch {
      case NonFatal(e) =>
        BookController.logger.error(s"Error getting books", e)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
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
