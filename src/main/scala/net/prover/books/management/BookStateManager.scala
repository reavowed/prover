package net.prover.books.management

import net.prover.books.model.Book
import net.prover.books.writing.WriteBooks
import net.prover.entries.GlobalContext
import net.prover.model.definitions.Definitions
import net.prover.model.EntryContext
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.stereotype.Service

import scala.concurrent.ExecutionContext

@Service
class BookStateManager {
  private var _globalContext: GlobalContext = GlobalContext(Nil, Definitions(EntryContext(Nil)))

  def globalContext: GlobalContext = _globalContext
  def books: Seq[Book] = _globalContext.allBooks

  def updateBooks[T](performUpdate: (Seq[Book] => GlobalContext) => T): T = {
    this.synchronized {
      var wasUpdated = false
      val result = performUpdate(newBooks => {
        val newDefinitions = Definitions(books)
        _globalContext = GlobalContext(newBooks.toList, newDefinitions)
        wasUpdated = true
        _globalContext
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
