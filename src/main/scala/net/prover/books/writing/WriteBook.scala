package net.prover.books.writing

import net.prover.books.keys.GetWithKeys
import net.prover.books.management.BookDirectoryConfig
import net.prover.books.model.{Book, FileDefinition}
import net.prover.controllers.BookService
import net.prover.model.entries.Theorem
import net.prover.model.Chapter
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter

import scala.collection.JavaConverters._

object WriteBook {
  def apply(book: Book): Unit = {
    val files = getBookFiles(book)
    files.foreach(WriteFile(_))
    deleteUnusedFiles(book, files)
  }

  private def getBookFiles(book: Book) = {
    FileDefinition(BookDirectoryConfig.getBookFilePath(book.title), book.serialized) +: getChapterFiles(book)
  }

  private def getChapterFiles(book: Book): Seq[FileDefinition] = {
    book.chapters.flatMapWithIndex(getChapterFiles(book, _, _))
  }

  private def getChapterFiles(book: Book, chapter: Chapter, chapterIndex: Int): Seq[FileDefinition] = {
    getChapterFile(book, chapter, chapterIndex) +: getProofFiles(book, chapter, chapterIndex)
  }

  private def getChapterFile(book: Book, chapter: Chapter, chapterIndex: Int): FileDefinition = {
    FileDefinition(
      BookDirectoryConfig.getChapterFilePath(book.title, chapter.title, chapterIndex),
      chapter.serialized)
  }

  private def getProofFiles(book: Book, chapter: Chapter, chapterIndex: Int): Seq[FileDefinition] = {
    GetWithKeys(chapter.entries).collect {
      case (theorem: Theorem, key) => (theorem, key)
    }.flatMapWithIndex { case ((theorem, key), index) =>
      theorem.proofs.mapWithIndex((proof, proofIndex) =>
        FileDefinition(
          BookDirectoryConfig.getProofPath(book.title, chapter.title, chapterIndex, key, index, proofIndex),
          proof.serialized))
    }
  }

  private def deleteUnusedFiles(book: Book, bookFiles: Seq[FileDefinition]): Unit = {
    FileUtils.listFiles(BookDirectoryConfig.getBookDirectoryPath(book.title).toFile, TrueFileFilter.INSTANCE, TrueFileFilter.INSTANCE).asScala.foreach { file =>
      if (!bookFiles.exists(_.path.toAbsolutePath.toString == file.getAbsolutePath)) {
        file.delete()
      }
    }
  }
}
