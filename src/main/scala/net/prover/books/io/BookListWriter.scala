package net.prover.books.io

import net.prover.model.Book

object BookListWriter {
  def write(books: Seq[Book]): Unit = {
    FileWriter.write(
      BookDirectoryConfig.bookListPath,
      books.map(_.title).mkString("\n") + "\n")
  }
}
