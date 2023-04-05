package net.prover.books.writing

import net.prover.books.management.BookDirectoryConfig
import net.prover.books.model.{Book, FileDefinition}
import net.prover.entries.{BookWithContext, ChapterWithContext}
import net.prover.model.{Chapter, SeqOps}
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter

import scala.collection.JavaConverters._

object WriteBook {
  def apply(bookWithContext: BookWithContext): Unit = {
    val files = getBookFiles(bookWithContext)
    files.foreach(WriteFile(_))
    deleteUnusedFiles(bookWithContext.book, files)
  }

  private def getBookFiles(bookWithContext: BookWithContext) = {
    FileDefinition(
      BookDirectoryConfig.getBookFilePath(bookWithContext.book.title),
      bookWithContext.book.serialized
    ) +: getChapterFiles(bookWithContext)
  }

  private def getChapterFiles(bookWithContext: BookWithContext): Seq[FileDefinition] = {
    bookWithContext.chaptersWithContexts.flatMapWithIndex(getChapterFiles(bookWithContext, _, _))
  }

  private def getChapterFiles(bookWithContext: BookWithContext, chapterWithContext: ChapterWithContext, chapterIndex: Int): Seq[FileDefinition] = {
    getChapterFile(bookWithContext.book, chapterWithContext.chapter, chapterIndex) +:
      getProofFiles(bookWithContext, chapterWithContext, chapterIndex)
  }

  private def getChapterFile(book: Book, chapter: Chapter, chapterIndex: Int): FileDefinition = {
    FileDefinition(
      BookDirectoryConfig.getChapterFilePath(book.title, chapter.title, chapterIndex),
      chapter.serialized)
  }

  private def getProofFiles(bookWithContext: BookWithContext, chapterWithContext: ChapterWithContext, chapterIndex: Int): Seq[FileDefinition] = {
    import bookWithContext.book
    import chapterWithContext.chapter
    chapterWithContext.theoremsWithContexts.flatMapWithIndex { (theoremWithContext, index) =>
      theoremWithContext.theorem.proofs.mapWithIndex((proof, proofIndex) =>
        FileDefinition(
          BookDirectoryConfig.getProofPath(book.title, chapter.title, chapterIndex, theoremWithContext.entryKey, index, proofIndex),
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
