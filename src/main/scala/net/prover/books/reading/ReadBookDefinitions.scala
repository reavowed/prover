package net.prover.books.reading

import net.prover.books.management.BookDirectoryConfig
import net.prover.books.model.BookDefinition

import java.nio.file.Files
import scala.collection.JavaConverters._

object ReadBookDefinitions {
  def apply(): Seq[BookDefinition] = {
    Files
      .readAllLines(BookDirectoryConfig.bookListPath)
      .asScala
      .filter(s => !s.startsWith("#"))
      .map(BookDefinition)
  }
}
