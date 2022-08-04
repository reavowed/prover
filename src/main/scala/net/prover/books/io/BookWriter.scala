package net.prover.books.io

import net.prover.model.{Book, Chapter}
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter

import java.nio.file.{Files, Path}
import scala.collection.JavaConverters._

object BookWriter {
  def write(book: Book): Unit = {
    val bookFilePath = BookDirectoryConfig.getBookPath(book.title)
    val directoryPath = bookFilePath.getParent
    val files = getBookFiles(book, bookFilePath)
    Files.createDirectories(directoryPath)
    files.foreach(FileWriter.write)
    deleteUnusedFiles(directoryPath, files)
  }

  private def getBookFiles(book: Book, bookFilePath: Path) = {
    FileDefinition(bookFilePath, book.serialized) +: getChapterFiles(book)
  }

  private def getChapterFiles(book: Book) = {
    book.chapters.mapWithIndex(getChapterFile(book, _, _))
  }

  private def getChapterFile(book: Book, chapter: Chapter, index: Int) = {
    FileDefinition(
      BookDirectoryConfig.getChapterPath(book.title, chapter.title, index),
      chapter.serialized)
  }

  private def deleteUnusedFiles(directoryPath: Path, bookFiles: Seq[FileDefinition]): Unit = {
    FileUtils.listFiles(directoryPath.toFile, TrueFileFilter.INSTANCE, null).asScala.foreach { file =>
      if (!bookFiles.exists(_.path.toAbsolutePath.toString == file.getAbsolutePath)) {
        file.delete()
      }
    }
  }
}
