package net.prover.model.entries

import net.prover.model.{Chapter, Parser, ParsingContext}

trait ChapterEntryParser[T <: ChapterEntry] {
  def name: String
  def parser(implicit context: ParsingContext): Parser[T]
  def addToContext(t: T, context: ParsingContext): ParsingContext = context

  def parseToChapter(chapter: Chapter, context: ParsingContext): Parser[(Chapter, ParsingContext)] = {
    for {
      t <- parser(context)
    } yield {
      (chapter.copy(entries = chapter.entries :+ t), context.add(t))
    }
  }
}
