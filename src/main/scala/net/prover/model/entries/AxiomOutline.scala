package net.prover.model.entries

import net.prover.model.Inference.{Premise, RearrangementType}
import net.prover.model._
import net.prover.model.components.Statement

case class AxiomOutline(
    name: String,
    premises: Seq[Premise],
    conclusion: ProvenStatement,
    rearrangementType: RearrangementType = RearrangementType.NotRearrangement,
    allowsRearrangement: Boolean = true)
  extends ChapterEntry(AxiomOutline)
{
  def expand(
    chapterTitle: String,
    bookTitle: String,
    nextInferenceKey: String => String
  ): Axiom = {
    Axiom(
      name,
      nextInferenceKey(name),
      chapterTitle.formatAsKey,
      chapterTitle,
      bookTitle.formatAsKey,
      bookTitle,
      premises,
      conclusion,
      rearrangementType,
      allowsRearrangement)
  }
}

object AxiomOutline extends ChapterEntryParser[AxiomOutline] with InferenceParser {
  override val name: String = "axiom"

  private def conclusionParser(implicit context: ParsingContext): Parser[Statement] = {
    for {
      _ <- Parser.requiredWord("conclusion")
      conclusion <- Statement.parser
    } yield conclusion
  }

  def parser(implicit context: ParsingContext): Parser[AxiomOutline] = {
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      allowsRearrangement <- Parser.optionalWord("disallow-rearrangement").isUndefined
      premises <- premisesParser
      conclusion <- conclusionParser
      conditions <- Conditions.parser
    } yield {
      AxiomOutline(
        name,
        premises,
        ProvenStatement(conclusion, conditions),
        rearrangementType,
        allowsRearrangement)
    }
  }
}
