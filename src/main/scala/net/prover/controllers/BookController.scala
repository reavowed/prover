package net.prover.controllers

import net.prover.model.Book
import org.slf4j.LoggerFactory
import org.springframework.http.{HttpStatus, ResponseEntity}
import org.springframework.web.bind.annotation.{GetMapping, RequestMapping, RestController}

import scala.util.control.NonFatal

@RestController
@RequestMapping(Array("/books"))
class BookController {

  def books = Book.fromDirectory("books")

  @GetMapping(Array(""))
  def get = {
    try {
      new ResponseEntity[Seq[Book]](books, HttpStatus.OK)
      books
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
