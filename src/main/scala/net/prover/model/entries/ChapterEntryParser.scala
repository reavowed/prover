package net.prover.model.entries

import net.prover.model._

trait ChapterEntryParser[T <: ChapterEntry] {
  def name: String
  def parser(chapterKey: String, bookKey: String)(implicit context: ParsingContext): Parser[T]

  def parseToChapter(chapter: Chapter, context: ParsingContext): Parser[(Chapter, ParsingContext)] = {
    for {
      t <- parser(chapter.key, chapter.bookKey)(context)
    } yield {
      (chapter.copy(entries = chapter.entries :+ t), context.add(t))
    }
  }
}
