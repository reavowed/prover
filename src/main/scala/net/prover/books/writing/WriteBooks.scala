package net.prover.books.writing

import net.prover.model.Book

object WriteBooks {
  def apply(books: Seq[Book]): Unit = {
    WriteBookDefinitions(books)
    books.foreach(WriteBook.apply)
  }
}
