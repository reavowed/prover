package net.prover.model.entries

import net.prover.model.{Book, Chapter, ChapterEntry, Context, Parser}

case class Comment(text: String) extends ChapterEntry(Comment)

object Comment extends ChapterEntryParser[Comment] {
  override val name: String = "comment"
  def parser(book: Book, chapter: Chapter)(implicit context: Context): Parser[Comment] = Parser.toEndOfLine.map(Comment.apply)
  override def addToContext(t: Comment, context: Context): Context = context
}
