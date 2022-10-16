package net.prover.books.io

import net.prover.model.entries.ChapterEntry
import net.prover.model.{Chapter, EntryContext, Parser}

object ChapterReader {
  def readChapter(
    bookOutline: BookOutline,
    chapterTitle: String,
    chapterIndex: Int,
    entryContext: EntryContext
  ): (Chapter, EntryContext) = {
    val filePath = BookDirectoryConfig.getChapterFilePath(bookOutline.title, chapterTitle, chapterIndex)
    val directoryPath = BookDirectoryConfig.getChapterDirectoryPath(bookOutline.title, chapterTitle, chapterIndex)
    val entryParsingContext = EntryParsingContext(entryContext, ProofFileReader(directoryPath, KeyAccumulator.Empty))
    val parser = for {
      summary <- Parser.toEndOfLine
      entriesAndContext <- Parser.foldWhileDefined[ChapterEntry, EntryParsingContext](entryParsingContext) { (_, _, currentContext) =>
        ChapterEntry.parser(currentContext).mapMap { entry =>
          (entry, currentContext.addEntry(entry))
        }
      }
    } yield entriesAndContext.mapLeft(Chapter(chapterTitle, summary, _))

    parser
      .parseFromFile(filePath, s"book '${bookOutline.title}' chapter '$chapterTitle''")
      .mapRight(_.entryContext)
  }
}
