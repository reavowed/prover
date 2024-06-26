package net.prover.books.management

import net.prover.books.keys.ListWithKeys
import net.prover.books.model.Book
import net.prover.books.writing.WriteBooks
import net.prover.entries.GlobalContext
import org.slf4j.{Logger, LoggerFactory}
import org.springframework.stereotype.Service

import scala.concurrent.ExecutionContext

@Service
class BookStateManager {
  private var _globalContext: GlobalContext = GlobalContext(ListWithKeys.empty)

  def globalContext: GlobalContext = _globalContext
  def books: Seq[Book] = _globalContext.allBooks

  def updateBooks[T](performUpdate: (ListWithKeys[Book] => GlobalContext) => T): T = {
    this.synchronized {
      var wasUpdated = false
      val oldContext = _globalContext
      val result = performUpdate(newBooks => {
        _globalContext = GlobalContext(newBooks)
        wasUpdated = true
        _globalContext
      })
      if (wasUpdated) {
        val newContext = _globalContext
        WriteBooks(oldContext.booksWithContexts, newContext.booksWithContexts)
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
