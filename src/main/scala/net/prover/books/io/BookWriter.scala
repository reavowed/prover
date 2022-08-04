package net.prover.books.io

import net.prover.model.{Book, Chapter}
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter

import scala.collection.JavaConverters._

object BookWriter {
  def write(book: Book): Unit = {
    val files = getBookFiles(book)
    files.foreach(FileWriter.write)
    deleteUnusedFiles(book, files)
  }

  private def getBookFiles(book: Book) = {
    FileDefinition(BookDirectoryConfig.getBookFilePath(book.title), book.serialized) +: getChapterFiles(book)
  }

  private def getChapterFiles(book: Book) = {
    book.chapters.mapWithIndex(getChapterFile(book, _, _))
  }

  private def getChapterFile(book: Book, chapter: Chapter, index: Int) = {
    FileDefinition(
      BookDirectoryConfig.getChapterPath(book.title, chapter.title, index),
      chapter.serialized)
  }

  private def deleteUnusedFiles(book: Book, bookFiles: Seq[FileDefinition]): Unit = {
    FileUtils.listFiles(BookDirectoryConfig.getBookDirectoryPath(book.title).toFile, TrueFileFilter.INSTANCE, null).asScala.foreach { file =>
      if (!bookFiles.exists(_.path.toAbsolutePath.toString == file.getAbsolutePath)) {
        file.delete()
      }
    }
  }
}
