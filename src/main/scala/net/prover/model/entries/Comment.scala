package net.prover.model.entries

import net.prover.model.{Parser, ParsingContext}

case class Comment(text: String) extends ChapterEntry(Comment)

object Comment extends ChapterEntryParser[Comment] {
  override val name: String = "comment"
  def parser(implicit context: ParsingContext): Parser[Comment] = Parser.toEndOfLine.map(Comment.apply)
}
