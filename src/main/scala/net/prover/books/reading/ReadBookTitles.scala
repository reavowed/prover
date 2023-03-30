package net.prover.books.reading

import net.prover.books.management.BookDirectoryConfig
import net.prover.books.model.BookTitle

import java.nio.file.Files
import scala.collection.JavaConverters._

object ReadBookTitles {
  def apply(): Seq[BookTitle] = {
    Files
      .readAllLines(BookDirectoryConfig.bookListPath)
      .asScala
      .filter(s => !s.startsWith("#"))
      .map(BookTitle)
  }
}
