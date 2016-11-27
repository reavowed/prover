package net.prover.model

case class Comment(text: String) extends ChapterEntry {
  val `type` = "comment"
}

object Comment extends SingleLineChapterEntryParser[Comment] {
  override val name: String = "comment"
  override def parse(line: PartialLine, book: Book): Comment = {
    Comment(line.remainingText)
  }
  override def addToBook(t: Comment, book: Book): Book = book
}
