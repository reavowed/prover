package net.prover.books.management

import net.prover.books.reading.{ReadBook, ReadBookDefinitions}
import net.prover.model._

object ReloadBooks {
  def apply()(implicit bookStateManager: BookStateManager): Unit = {
    bookStateManager.updateBooks(setBooks => {
      val bookDefinitions = ReadBookDefinitions()
      val books = bookDefinitions.mapReduceWithPrevious[Book] { case (booksSoFar, bookDefinition) =>
        val newBook = ReadBook(bookDefinition, booksSoFar)
        setBooks(booksSoFar :+ newBook)
        newBook
      }
      BookStateManager.logger.info(s"Parsed ${books.length} books")
    })
  }
}
