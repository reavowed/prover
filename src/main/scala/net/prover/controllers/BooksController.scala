package net.prover.controllers

import net.prover.controllers.BooksController.BookDefinition
import net.prover.controllers.models.LinkSummary
import net.prover.model.Book
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.http.{HttpStatus, ResponseEntity}
import org.springframework.web.bind.annotation.{GetMapping, PostMapping, RequestBody, RequestMapping, RestController}

@RestController
@RequestMapping(Array("/books"))
class BooksController @Autowired() (val bookService: BookService) extends BookModification with ReactViews {

  case class BooksProps(books: Seq[LinkSummary])
  def createBooksProps(books: Seq[Book]): BooksProps = {
    BooksProps(bookService.getBooksWithKeys.map { case (book, key) => LinkSummary(book.title, BookService.getBookUrl(key)) })
  }

  @GetMapping(produces = Array("text/html;charset=UTF-8"))
  def get: ResponseEntity[_] = {
    new ResponseEntity(createReactView("Books", createBooksProps(bookService.books)), HttpStatus.OK)
  }

  @GetMapping(value = Array("reloadFromDisk"))
  def reloadFromDisk(): Unit = {
    bookService.reload().toResponseEntity
  }

  @PostMapping(produces = Array("application/json;charset=UTF-8"))
  def createBook(@RequestBody definition: BookDefinition): ResponseEntity[_] = {
    val currentBooks = bookService.books
    def validateImport(title: String): Unit = {
      if (!currentBooks.exists(_.title == title)) {
        throw new Exception(s"Could not find import $title")
      }
    }
    (for {
      _ <- definition.imports.foreach(validateImport).recoverWithBadRequest
      (newBooks, _) = bookService.modifyBooks[Identity] { (books, _) =>
        books :+ Book(definition.title, definition.imports, Nil, Nil)
      }
    } yield createBooksProps(newBooks)).toResponseEntity
  }
}
object BooksController {
  case class BookDefinition(title: String, imports: Seq[String])
}
