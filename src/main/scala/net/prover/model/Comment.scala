package net.prover.model

case class Comment(text: String) extends ChapterEntry(Comment)

object Comment extends SingleLineChapterEntryParser[Comment] {
  override val name: String = "comment"
  def parser(context: Context): Parser[Comment] = Parser.allRemaining.map(Comment.apply)
  override def addToContext(t: Comment, context: Context): Context = context
}
