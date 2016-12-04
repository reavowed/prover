package net.prover.model

case class Comment(text: String) extends ChapterEntry(Comment)

object Comment extends SingleLineChapterEntryParser[Comment] {
  override val name: String = "comment"
  override def parse(line: PartialLine, context: Context): Comment = {
    Comment(line.remainingText)
  }
  override def addToContext(t: Comment, context: Context): Context = context
}
