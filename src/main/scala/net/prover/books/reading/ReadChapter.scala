package net.prover.books.reading

import net.prover.books.keys.ListWithKeys
import net.prover.books.management.BookDirectoryConfig
import net.prover.entries.{BookWithContext, ChapterWithContext}
import net.prover.model.entries.ChapterEntry
import net.prover.model.{Chapter, EntryContext, Parser}

object ReadChapter {
  def apply(
    bookWithContext: BookWithContext,
    chapterTitle: String,
    chapterIndex: Int
  ): Chapter = {
    import bookWithContext.book
    val filePath = BookDirectoryConfig.getChapterFilePath(book.title, chapterTitle, chapterIndex)
    val chapterDirectoryPath = BookDirectoryConfig.getChapterDirectoryPath(book.title, chapterTitle, chapterIndex)
    val parser = for {
      summary <- Parser.toEndOfLine
      initialChapter = Chapter(chapterTitle, summary, ListWithKeys.empty)
      chapterKey = bookWithContext.book.chaptersWithKeys.keyAccumulator.getNextKey(initialChapter)._1
      chapter <- Parser.foldWhileDefined[Chapter](initialChapter) { chapter: Chapter =>
        val chapterWithContext = ChapterWithContext(chapter, chapterKey, bookWithContext)
        val entryContext = EntryContext.forChapterInclusive(chapterWithContext)
        val proofFileReader = ProofFileReader(chapterDirectoryPath, chapter.entriesWithKeys.keyAccumulator)
        ChapterEntry.parser(entryContext, proofFileReader).mapMap {chapter.addEntry}
      }
    } yield chapter

    parser.parseFromFile(filePath, s"book '${bookWithContext.book.title}' chapter '$chapterTitle''")
  }
}
