package net.prover.books.io

import java.nio.file.{Path, Paths}
import net.prover.model._

object BookDirectoryConfig {
  val bookDirectoryPath: Path = Paths.get("books")
  val bookListPath: Path = bookDirectoryPath.resolve("books.list")

  def getBookPath(title: String): Path = {
    val key = title.formatAsKey
    bookDirectoryPath.resolve(key).resolve(key + ".book")
  }

  def getChapterPath(bookTitle: String, chapterTitle: String, chapterIndex: Int): Path = {
    bookDirectoryPath.resolve(bookTitle.formatAsKey).resolve("%02d".format(chapterIndex + 1) + "." + chapterTitle.camelCase + ".chapter")
  }
}
