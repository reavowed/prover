package net.prover.books.io

import java.nio.file.Files
import scala.collection.JavaConverters._

object BookListReader {
  def read(): BookList = {
    val bookDefinitions = Files
      .readAllLines(BookDirectoryConfig.bookListPath)
      .asScala
      .filter(s => !s.startsWith("#"))
      .map(BookDefinition)
    BookList(bookDefinitions)
  }
}
