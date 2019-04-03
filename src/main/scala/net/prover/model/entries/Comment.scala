package net.prover.model.entries

import net.prover.model.{EntryContext, Parser}

case class Comment(text: String) extends ChapterEntry {
  override def name: String = Comment.name
  override def serializedLines: Seq[String] = Seq(s"comment $text")

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedDefinitions: Set[ExpressionDefinition] = Set.empty
}

object Comment extends ChapterEntryParser {
  override val name: String = "comment"
  override def parser(implicit context: EntryContext): Parser[Comment] = {
    for {
      text <- Parser.toEndOfLine
    } yield Comment(text)
  }
}
