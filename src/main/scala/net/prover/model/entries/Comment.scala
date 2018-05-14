package net.prover.model.entries

import net.prover.model.{Parser, ParsingContext}

case class Comment(text: String) extends ChapterEntry {
  override def serializedLines: Seq[String] = Seq(s"comment $text")
}

object Comment extends ChapterEntryParser.WithoutKey {
  override val name: String = "comment"
  def parser(chapterTitle: String, bookTitle: String)(implicit context: ParsingContext): Parser[Comment] = Parser.toEndOfLine.map(Comment.apply)
}
