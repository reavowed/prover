package net.prover.books.writing

import net.prover.entries.BookWithContext

object WriteBooks {
  def apply(books: Seq[BookWithContext]): Unit = {
    WriteBookDefinitions(books.map(_.book))
    books.foreach(WriteBook.apply)
  }
}
