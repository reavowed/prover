package net.prover.books.io

import java.nio.file.{Path, Paths}
import net.prover.model._

object BookDirectoryConfig {
  val booksDirectoryPath: Path = Paths.get("books")
  val bookListPath: Path = booksDirectoryPath.resolve("books.list")

  def getBookDirectoryPath(bookTitle: String): Path = {
    booksDirectoryPath.resolve(bookTitle.formatAsKey)
  }
  def getBookFilePath(bookTitle: String): Path = {
    getBookDirectoryPath(bookTitle).resolve(bookTitle.formatAsKey + ".book")
  }

  private def getChapterKey(chapterTitle: String, chapterIndex: Int): String = {
    "%02d".format(chapterIndex + 1) + "." + chapterTitle.camelCase
  }
  private def getChapterDirectoryPath(bookTitle: String, chapterTitle: String, chapterIndex: Int): Path = {
    getBookDirectoryPath(bookTitle).resolve(getChapterKey(chapterTitle, chapterIndex))
  }
  def getChapterPath(bookTitle: String, chapterTitle: String, chapterIndex: Int): Path = {
    getChapterDirectoryPath(bookTitle, chapterTitle, chapterIndex).resolve(getChapterKey(chapterTitle, chapterIndex) + ".chapter")
  }
}
