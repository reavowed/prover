package net.prover.books.writing

import net.prover.books.management.BookDirectoryConfig
import net.prover.books.model.Book

object WriteBookDefinitions {
  def apply(books: Seq[Book]): Unit = {
    WriteFile(
      BookDirectoryConfig.bookListPath,
      books.map(_.title).mkString("\n") + "\n")
  }
}
