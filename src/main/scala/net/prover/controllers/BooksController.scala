package net.prover.controllers

import net.prover.books.management.{CreateBook, BookStateManager, ReloadBooks}
import net.prover.books.model.BookDefinition
import net.prover.controllers.models.LinkSummary
import net.prover.entries.GlobalContext
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.{HttpStatus, ResponseEntity}
import org.springframework.web.bind.annotation._

import scala.util.Try

@RestController
@RequestMapping(Array("/books"))
class BooksController @Autowired() (implicit val bookStateManager: BookStateManager) extends ReactViews {

  case class BooksProps(books: Seq[LinkSummary])
  private def createBooksProps(globalContext: GlobalContext): BooksProps = {
    BooksProps(globalContext.booksWithKeys.map { case (book, key) => LinkSummary(book.title, BookService.getBookUrl(key)) })
  }

  @GetMapping(produces = Array("text/html;charset=UTF-8"))
  def get: ResponseEntity[_] = {
    new ResponseEntity(createReactView("Books", createBooksProps(bookStateManager.globalContext)), HttpStatus.OK)
  }

  @GetMapping(value = Array("reloadFromDisk"))
  def reloadFromDisk(): Unit = {
    Try(ReloadBooks()).toResponseEntity
  }

  @PostMapping(produces = Array("application/json;charset=UTF-8"))
  def createBook(@RequestBody definition: BookDefinition): ResponseEntity[_] = {
    CreateBook(definition).map(createBooksProps).toResponseEntity
  }
}
