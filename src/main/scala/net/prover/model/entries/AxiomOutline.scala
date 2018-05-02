package net.prover.model.entries

import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.expressions.Statement

case class AxiomOutline(
    name: String,
    premises: Seq[Premise],
    conclusion: Statement,
    rearrangementType: RearrangementType = RearrangementType.NotRearrangement)
  extends ChapterEntryOutline
{
  def expand(
    key: String,
    chapterTitle: String,
    bookTitle: String
  ): Axiom = {
    Axiom(
      name,
      key,
      chapterTitle.formatAsKey,
      chapterTitle,
      bookTitle.formatAsKey,
      bookTitle,
      premises,
      conclusion,
      rearrangementType)
  }
}

object AxiomOutline extends ChapterEntryParser {
  override val name: String = "axiom"

  private def conclusionParser(implicit context: ParsingContext): Parser[Statement] = {
    for {
      _ <- Parser.requiredWord("conclusion")
      conclusion <- Statement.parser
    } yield conclusion
  }

  def parser(chapterKey: String, bookKey: String)(implicit context: ParsingContext): Parser[AxiomOutline] = {
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      premises <- Premise.listParser
      conclusion <- conclusionParser
    } yield {
      AxiomOutline(
        name,
        premises,
        conclusion,
        rearrangementType)
    }
  }
}
