package net.prover.books.writing

import net.prover.books.model.Book

object WriteBooks {
  def apply(books: Seq[Book]): Unit = {
    WriteBookDefinitions(books)
    books.foreach(WriteBook.apply)
  }
}
