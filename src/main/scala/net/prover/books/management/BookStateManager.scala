package net.prover.books.management

import net.prover.books.model.Book
import net.prover.books.writing.WriteBooks
import net.prover.model.definitions.Definitions
import net.prover.model.EntryContext
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.stereotype.Service

import scala.concurrent.ExecutionContext

@Service
class BookStateManager {
  private var _booksAndDefinitions: (Seq[Book], Definitions) = (Nil, Definitions(EntryContext(Nil)))

  def booksAndDefinitions: (Seq[Book], Definitions) = _booksAndDefinitions
  def books: Seq[Book] = booksAndDefinitions._1

  def updateBooks[T](performUpdate: (Seq[Book] => (Seq[Book], Definitions)) => T): T = {
    this.synchronized {
      var wasUpdated = false
      val result = performUpdate(newBooks => {
        val newDefinitions = Definitions(books)
        _booksAndDefinitions = (newBooks, newDefinitions)
        wasUpdated = true
        _booksAndDefinitions
      })
      if (wasUpdated) {
        WriteBooks(books)
      }
      result
    }
  }

  ExecutionContext.global.execute(() => {
    ReloadBooks()(this)
  })

}

object BookStateManager {
  val logger: Logger = LoggerFactory.getLogger(BookStateManager.getClass)
}
