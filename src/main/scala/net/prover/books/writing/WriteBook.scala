package net.prover.books.writing

import net.prover.books.management.BookDirectoryConfig
import net.prover.books.model.Book
import net.prover.entries.{BookWithContext, ChapterWithContext, TheoremWithContext}
import net.prover.model.{Chapter, SeqOps}
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter

import java.nio.file.{Files, Path}
import scala.collection.JavaConverters._

object WriteBook {

  case class FileDefinition(path: Path, contentsToWrite: Option[String])

  def apply(newBookWithContext: BookWithContext, oldBookWithContext: Option[BookWithContext]): Unit = {
    if (!oldBookWithContext.exists(_.book eq newBookWithContext.book)) {
      val files = getBookFiles(newBookWithContext, oldBookWithContext)
      files.foreach(writeFile)
      deleteUnusedFiles(newBookWithContext.book, files)
    }
  }

  private def getBookFiles(newBookWithContext: BookWithContext, oldBookWithContext: Option[BookWithContext]): Seq[FileDefinition] = {
    getMainBookFile(newBookWithContext, oldBookWithContext) +: getChapterFiles(newBookWithContext, oldBookWithContext)
  }

  private def getMainBookFile(newBookWithContext: BookWithContext, oldBookWithContext: Option[BookWithContext]): FileDefinition = {
    FileDefinition(
      BookDirectoryConfig.getBookFilePath(newBookWithContext.book.title),
      Some(newBookWithContext.book.serialized).filter(newSerializedBook => !oldBookWithContext.exists(_.book.serialized == newSerializedBook)))
  }

  private def getChapterFiles(newBookWithContext: BookWithContext, oldBookWithContext: Option[BookWithContext]): Seq[FileDefinition] = {
    newBookWithContext.chaptersWithContexts.flatMapWithIndex { (newChapter, index) =>
      val oldChapter = oldBookWithContext.flatMap(_.chaptersWithContexts.lift(index))
      getChapterFiles(newChapter, oldChapter, index)
    }
  }

  private def getChapterFiles(chapterWithContext: ChapterWithContext, oldChapterWithContext: Option[ChapterWithContext], chapterIndex: Int): Seq[FileDefinition] = {
    val isWriteCheckRequired = !oldChapterWithContext.exists(_.chapter eq chapterWithContext.chapter)
    getChapterFile(chapterWithContext, oldChapterWithContext.filter(_ => isWriteCheckRequired), chapterIndex, isWriteCheckRequired) +:
      getProofFiles(chapterWithContext, oldChapterWithContext.filter(_ => isWriteCheckRequired), chapterIndex, isWriteCheckRequired)
  }

  private def getChapterFile(chapterWithContext: ChapterWithContext, oldChapterWithContext: Option[ChapterWithContext], chapterIndex: Int, isWriteCheckRequired: Boolean): FileDefinition = {
    import chapterWithContext._
    FileDefinition(
      BookDirectoryConfig.getChapterFilePath(bookWithContext.book.title, chapter.title, chapterIndex),
      if (isWriteCheckRequired)
        Some(chapter.serialized).filter(contents => !oldChapterWithContext.exists(_.chapter.serialized == contents))
      else None)
  }

  private def getProofFiles(chapterWithContext: ChapterWithContext, oldChapterWithContext: Option[ChapterWithContext], chapterIndex: Int, isWriteCheckRequired: Boolean): Seq[FileDefinition] = {
    import chapterWithContext._
    chapterWithContext.theoremsWithContexts.flatMapWithIndex { (theoremWithContext, theoremIndex) =>
      val oldTheoremWithContext = oldChapterWithContext.flatMap(_.theoremsWithContexts.lift(theoremIndex))
      val isTheoremWriteCheckRequired = isWriteCheckRequired && !oldTheoremWithContext.exists(_.theorem eq theoremWithContext.theorem)
      getProofFiles(theoremWithContext, oldTheoremWithContext.filter(_ => isTheoremWriteCheckRequired), chapterIndex, theoremIndex, isTheoremWriteCheckRequired)
    }
  }

  private def getProofFiles(theoremWithContext: TheoremWithContext, oldTheoremWithContext: Option[TheoremWithContext], chapterIndex: Int, theoremIndex: Int, isWriteCheckRequired: Boolean): Seq[FileDefinition] = {
    import theoremWithContext._, chapterWithContext._, bookWithContext._
    theoremWithContext.theorem.proofs.mapWithIndex { (proof, proofIndex) =>
      val oldProof = oldTheoremWithContext.flatMap(_.theorem.proofs.lift(proofIndex))
      val isProofWriteCheckRequired = isWriteCheckRequired && !oldProof.exists(_ eq proof)
      FileDefinition(
        BookDirectoryConfig.getProofPath(book.title, chapter.title, chapterIndex, theoremWithContext.entryKey, theoremIndex, proofIndex),
        if (isProofWriteCheckRequired) Some(proof.serialized).filter(s => !oldProof.exists(_.serialized == s)) else None)
    }
  }

  private def deleteUnusedFiles(book: Book, bookFiles: Seq[FileDefinition]): Unit = {
    FileUtils.listFiles(BookDirectoryConfig.getBookDirectoryPath(book.title).toFile, TrueFileFilter.INSTANCE, TrueFileFilter.INSTANCE).asScala.foreach { file =>
      if (!bookFiles.exists(_.path.toAbsolutePath.toString == file.getAbsolutePath)) {
        file.delete()
      }
    }
  }

  def writeFile(fileDefinition: FileDefinition): Unit = {
    fileDefinition.contentsToWrite.foreach { WriteFile(fileDefinition.path, _) }
  }
}
