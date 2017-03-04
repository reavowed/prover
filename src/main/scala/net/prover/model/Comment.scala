package net.prover.model

case class Comment(text: String) extends ChapterEntry(Comment)

object Comment extends ChapterEntryParser[Comment] {
  override val name: String = "comment"
  def parser(implicit context: Context): Parser[Comment] = Parser.toEndOfLine.map(Comment.apply)
  override def addToContext(t: Comment, context: Context): Context = context
}
