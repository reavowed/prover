package net.prover.model.entries

import net.prover.model.{EntryContext, Parser}

case class Comment(text: String) extends ChapterEntry {
  override def name: String = Comment.name
  override def serializedLines: Seq[String] = Seq(s"comment $text")

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedDefinitions: Set[ChapterEntry] = Set.empty
  override def replaceDefinition(
    oldDefinition: ExpressionDefinition,
    newDefinition: ExpressionDefinition,
    entryContext: EntryContext
  ): Comment = this
}

object Comment extends ChapterEntryParser {
  override val name: String = "comment"
  override def parser(implicit context: EntryContext): Parser[Comment] = {
    for {
      text <- Parser.toEndOfLine
    } yield Comment(text)
  }
}
