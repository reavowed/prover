package net.prover.controllers

import net.prover.model.{Book, Chapter, Inference}
import org.slf4j.LoggerFactory
import org.springframework.http.{HttpStatus, ResponseEntity}
import org.springframework.web.bind.annotation.{GetMapping, PathVariable, RequestMapping, RestController}

import scala.util.control.NonFatal

@RestController
@RequestMapping(Array("/books"))
class BookController {
  private var _books: Option[Seq[Book]] = None
  def books: Seq[Book] = {
    val theoremCache = _books.map(_.flatMap(_.theoremCache).toMap).getOrElse(Map.empty)
    val newBooks = Book.fromDirectory("books", theoremCache)
    _books = Some(newBooks)
    newBooks
  }

  @GetMapping(Array(""))
  def get = {
    try {
      new ResponseEntity[Seq[Book]](books, HttpStatus.OK)
    } catch {
      case NonFatal(e) =>
        BookController.logger.error("Error getting books\n{}", e.getMessage)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @GetMapping(Array("/{bookKey}"))
  def getBook(@PathVariable("bookKey") bookKey: String) = {
    try {
      books.find(_.key == bookKey) match {
        case Some(book) =>
          new ResponseEntity[Book](book, HttpStatus.OK)
        case None =>
          new ResponseEntity(HttpStatus.NOT_FOUND)
      }
    } catch {
      case NonFatal(e) =>
        BookController.logger.error("Error getting books\n{}", e.getMessage)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @GetMapping(Array("/{bookKey}/{chapterKey}"))
  def getChapter(@PathVariable("bookKey") bookKey: String, @PathVariable("chapterKey") chapterKey: String) = {
    try {
      books.find(_.key == bookKey).flatMap(_.chapters.find(_.key == chapterKey)) match {
        case Some(chapter) =>
          new ResponseEntity[Chapter](chapter, HttpStatus.OK)
        case None =>
          new ResponseEntity(HttpStatus.NOT_FOUND)
      }
    } catch {
      case NonFatal(e) =>
        BookController.logger.error("Error getting books\n{}", e.getMessage)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }

  @GetMapping(Array("/{bookKey}/{chapterKey}/{inferenceKey}"))
  def getInference(
    @PathVariable("bookKey") bookKey: String,
    @PathVariable("chapterKey") chapterKey: String,
    @PathVariable("inferenceKey") inferenceKey: String
  ) = {
    try {
      books.find(_.key == bookKey)
        .flatMap(_.chapters.find(_.key == chapterKey))
        .flatMap(_.entries.ofType[Inference].find(_.keyOption.contains(inferenceKey)))  match {
        case Some(inference) =>
          new ResponseEntity[Inference](inference, HttpStatus.OK)
        case None =>
          new ResponseEntity(HttpStatus.NOT_FOUND)
      }
    } catch {
      case NonFatal(e) =>
        BookController.logger.error("Error getting books\n{}", e.getMessage)
        new ResponseEntity[Throwable](e, HttpStatus.INTERNAL_SERVER_ERROR)
    }
  }
}

object BookController {
  val logger = LoggerFactory.getLogger(BookController.getClass)
}
