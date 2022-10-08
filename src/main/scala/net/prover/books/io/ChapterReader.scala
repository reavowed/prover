package net.prover.books.io

import net.prover.model.entries.ChapterEntry
import net.prover.model.{Chapter, EntryContext, EntryParsingContext, Parser}

object ChapterReader {
  def readChapter(
    bookOutline: BookOutline,
    chapterTitle: String,
    chapterIndex: Int,
    entryContext: EntryContext
  ): (Chapter, EntryContext) = {
    val path = BookDirectoryConfig.getOldChapterFilePath(bookOutline.title, chapterTitle, chapterIndex)
    val entryParsingContext = EntryParsingContext(entryContext)
    val parser = for {
      summary <- Parser.toEndOfLine
      entriesAndContext <- Parser.foldWhileDefined[ChapterEntry, EntryParsingContext](entryParsingContext) { (_, _, currentContext) =>
        ChapterEntry.parser(currentContext).mapMap { entry =>
          (entry, currentContext.addEntry(entry))
        }
      }
    } yield entriesAndContext.mapLeft(Chapter(chapterTitle, summary, _))

    parser
      .parseFromFile(path, s"book '${bookOutline.title}' chapter '$chapterTitle''")
      .mapRight(_.entryContext)
  }
}
