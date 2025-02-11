package net.prover.books.reading

import net.prover.books.keys.ListWithKeys
import net.prover.books.management.BookDirectoryConfig
import net.prover.books.model.Book
import net.prover.entries.{BookWithContext, EntryParsingContext, GlobalContext}
import net.prover.model.entries.ChapterEntry
import net.prover.model.{AvailableEntries, Chapter}
import net.prover.parsing.{KnownWordParser, Parser}

object ReadChapter {
  def apply(
    bookSoFar: Book,
    previousBooks: ListWithKeys[Book],
    chapterTitle: String,
    chapterIndex: Int
  ): Chapter = {
    val filePath = BookDirectoryConfig.getChapterFilePath(bookSoFar.title, chapterTitle, chapterIndex)
    val chapterDirectoryPath = BookDirectoryConfig.getChapterDirectoryPath(bookSoFar.title, chapterTitle, chapterIndex)
    val parser = for {
      summary <- Parser.toEndOfLine
      initialChapter = Chapter(chapterTitle, summary, ListWithKeys.empty)
      chapter <- KnownWordParser.foldWhileDefined[Chapter](initialChapter) { chapter: Chapter =>
        val updatedBook = bookSoFar.addChapter(chapter)
        val (updatedBooks, updatedBookKey) = previousBooks.addAndGetKey(updatedBook)
        val bookWithContext = BookWithContext(updatedBook, updatedBookKey, GlobalContext(updatedBooks))
        val chapterWithContext = bookWithContext.getChapter(chapter)
        val availableEntries = AvailableEntries.forChapterInclusive(chapterWithContext)
        val proofFileReader = ProofFileReader(chapterDirectoryPath, chapter.entriesWithKeys.keyAccumulator)
        val entryParsingContext = EntryParsingContext(bookWithContext.book.title, chapterTitle, proofFileReader)(availableEntries)
        ChapterEntry.parser(entryParsingContext).map {chapter.addEntry}
      }
    } yield chapter

    parser.parseFromFile(filePath, s"book '${bookSoFar.title}' chapter '$chapterTitle''")
  }
}
