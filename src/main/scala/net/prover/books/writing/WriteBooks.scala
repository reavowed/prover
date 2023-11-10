package net.prover.books.writing

import net.prover.entries.BookWithContext

object WriteBooks {
  def apply(oldBooks: Seq[BookWithContext], newBooks: Seq[BookWithContext]): Unit = {
    WriteBookDefinitions(oldBooks, newBooks)
    newBooks.foreach { newBookWithContext =>
      WriteBook(newBookWithContext, oldBooks.find(_.book.title == newBookWithContext.book.title))
    }
  }
}
