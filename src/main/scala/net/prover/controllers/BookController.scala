package net.prover.controllers

import net.prover.model.{Book, Chapter}
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

  @GetMapping(Array("/{bookName}"))
  def getBook(@PathVariable("bookName") bookName: String) = {
    try {
      books.find(_.key == bookName) match {
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

  @GetMapping(Array("/{bookName}/{chapterName}"))
  def getChapter(@PathVariable("bookName") bookName: String, @PathVariable("chapterName") chapterName: String) = {
    try {
      books.find(_.key == bookName).flatMap(_.chapters.find(_.key == chapterName)) match {
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
}

object BookController {
  val logger = LoggerFactory.getLogger(BookController.getClass)
}
