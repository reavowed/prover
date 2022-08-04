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
    val path = BookDirectoryConfig.getChapterPath(bookOutline.title, chapterTitle, chapterIndex)
    val parser = for {
      summary <- Parser.toEndOfLine
      entriesAndContext <- Parser.foldWhileDefined[ChapterEntry, EntryContext](entryContext) { (_, _, currentContext) =>
        ChapterEntry.parser(currentContext).mapMap { entry =>
          (entry, currentContext.addEntry(entry))
        }
      }
    } yield entriesAndContext.mapLeft(Chapter(chapterTitle, summary, _))

    parser.parseFromFile(path, s"book '${bookOutline.title}' chapter '$chapterTitle''")
  }

}
