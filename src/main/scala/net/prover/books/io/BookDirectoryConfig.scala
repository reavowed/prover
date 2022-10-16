package net.prover.books.io

import java.nio.file.{Path, Paths}
import net.prover.model._
import net.prover.model.entries.Theorem

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
  def getChapterDirectoryPath(bookTitle: String, chapterTitle: String, chapterIndex: Int): Path = {
    getBookDirectoryPath(bookTitle).resolve(getChapterKey(chapterTitle, chapterIndex))
  }
  def getChapterFilePath(bookTitle: String, chapterTitle: String, chapterIndex: Int): Path = {
    getChapterDirectoryPath(bookTitle, chapterTitle, chapterIndex).resolve(chapterTitle.camelCase + ".chapter")
  }
  def getProofPath(bookTitle: String, chapterTitle: String, chapterIndex: Int, theoremKey: String, theoremIndex: Int, proofIndex: Int): Path = {
    getChapterDirectoryPath(bookTitle, chapterTitle, chapterIndex).resolve("%02d".format(theoremIndex + 1) + "." + theoremKey + "." + "%02d".format(proofIndex + 1) + ".proof")
  }
}
