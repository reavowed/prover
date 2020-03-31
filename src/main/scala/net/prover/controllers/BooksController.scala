package net.prover.controllers

import net.prover.controllers.BooksController.BookDefinition
import net.prover.controllers.models.LinkSummary
import net.prover.model.entries.{ChapterEntry, TermDefinition, WritingShorthand}
import net.prover.model.{Book, Chapter, EntryContext, Inference}
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

  @GetMapping(value = Array("replaceZero"))
  def replaceZero(): Unit = {
    def modifyEntry(tuple: (Map[String, String], EntryContext), chapterEntry: ChapterEntry): ((Map[String, String], EntryContext), ChapterEntry) = {
      val (changedInferences, entryContext) = tuple
      val (updatedInferences, newEntry) = if (chapterEntry.asOptionalInstanceOf[TermDefinition].exists(_.symbol == "0_ℤ")) {
        (changedInferences,  WritingShorthand.parser(entryContext).parseFromString("apply ⍳_ℤ 0 as 0_ℤ", ""))
      } else {
        val serializedEntry = chapterEntry.serializedLines.mkString("\n")
        val replacedSerializedEntry = changedInferences.foldLeft(serializedEntry) { case (currentSerializedEntry, (oldId, newId)) =>
          currentSerializedEntry.replace(oldId, newId)
        }
        val newEntry = Chapter.chapterEntryParser(entryContext).parseFromString(replacedSerializedEntry, chapterEntry.name).get
        val updatedInferences = changedInferences ++ chapterEntry.inferences.map(_.id).zip(newEntry.inferences.map(_.id)).filter { case (o, n) => o != n }.toMap
        (updatedInferences, newEntry)
      }
      ((updatedInferences, entryContext.addEntry(newEntry)), newEntry)
    }
    def modifyChapter(tuple: (Map[String, String], EntryContext), chapter: Chapter): ((Map[String, String], EntryContext), Chapter) = {
      println("  - " + chapter.title)
      chapter.entries.mapFold(tuple)(modifyEntry)
        .mapRight(entries => chapter.copy(entries = entries))
    }
    def modifyBook(changedInferences: Map[String, String], previousBooks: Seq[Book], book: Book): (Map[String, String], Book) = {
      println("- " + book.title)
      val entryContext = EntryContext.forBookExclusive(previousBooks, book)
      book.chapters.mapFold((changedInferences, entryContext))(modifyChapter)
        .mapRight(chapters => book.copy(chapters = chapters))
        .mapLeft(_._1)
    }
    println("Replacing")
    bookService.modifyBooks[Identity] { (books, _) =>
      books.mapFoldWithPrevious(Map.empty[String, String])(modifyBook)._2
    }
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
        books :+ Book(definition.title, definition.imports, Nil)
      }
    } yield createBooksProps(newBooks)).toResponseEntity
  }


}
object BooksController {
  case class BookDefinition(title: String, imports: Seq[String])
}
