package net.prover.books.reading

import net.prover.books.management.BookDirectoryConfig
import net.prover.books.model.BookTitle

import java.nio.file.Files
import scala.jdk.CollectionConverters._

object ReadBookDefinitions {
  def apply(): Seq[BookTitle] = {
    Files
      .readAllLines(BookDirectoryConfig.bookListPath)
      .asScala.toList
      .filter(s => !s.startsWith("#"))
      .map(BookTitle)
  }
}
