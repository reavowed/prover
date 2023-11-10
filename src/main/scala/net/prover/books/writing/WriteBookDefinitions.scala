package net.prover.books.writing

import net.prover.books.management.BookDirectoryConfig
import net.prover.entries.BookWithContext

object WriteBookDefinitions {
  def apply(oldBooks: Seq[BookWithContext], newBooks: Seq[BookWithContext]): Unit = {
    val oldTitles = oldBooks.map(_.book.title)
    val newTitles = newBooks.map(_.book.title)
    if (oldTitles != newTitles) {
      WriteFile(
        BookDirectoryConfig.bookListPath,
        newTitles.mkString("\n") + "\n")
    }
  }
}
