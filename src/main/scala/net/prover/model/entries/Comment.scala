package net.prover.model.entries

import net.prover.model.{Chapter, Parser, ParsingContext}

case class Comment(text: String, key: ChapterEntry.Key.Anchor) extends ChapterEntry {
  override def name: String = Comment.name
  override def serializedLines: Seq[String] = Seq(s"comment $text")
}

object Comment extends ChapterEntryParser {
  override val name: String = "comment"
  override def parser(getKey: String => (String, Chapter.Key))(implicit context: ParsingContext): Parser[Comment] = {
    for {
      text <- Parser.toEndOfLine
    } yield Comment(text, ChapterEntry.Key.Anchor(name, getKey))
  }
}
