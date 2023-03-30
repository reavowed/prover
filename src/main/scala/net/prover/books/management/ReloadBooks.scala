package net.prover.books.management

import net.prover.books.keys.ListWithKeys
import net.prover.books.model.Book
import net.prover.books.reading.{ReadBook, ReadBookDefinitions}

object ReloadBooks {
  def apply()(implicit bookStateManager: BookStateManager): Unit = {
    bookStateManager.updateBooks(setBooks => {
      val bookDefinitions = ReadBookDefinitions()
      val books = bookDefinitions.foldLeft(ListWithKeys.empty[Book]) { case (booksSoFar, bookDefinition) =>
        val newBooks = booksSoFar :+ ReadBook(bookDefinition, booksSoFar)
        setBooks(newBooks)
        newBooks
      }.list
      BookStateManager.logger.info(s"Parsed ${books.length} books")
    })
  }
}
