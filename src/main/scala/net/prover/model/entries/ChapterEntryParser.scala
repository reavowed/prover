package net.prover.model.entries

import net.prover.model._

trait ChapterEntryParser {
  def name: String
  def parser(chapterKey: String, bookKey: String)(implicit context: ParsingContext): Parser[ChapterEntryOutline]

  def parseToChapterOutline(chapterOutline: ChapterOutline, context: ParsingContext): Parser[(ChapterOutline, ParsingContext)] = {
    for {
      t <- parser(chapterOutline.key, chapterOutline.bookKey)(context)
    } yield {
      (chapterOutline.copy(entryOutlines = chapterOutline.entryOutlines :+ t), context.add(t))
    }
  }
}
