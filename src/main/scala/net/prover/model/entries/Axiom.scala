package net.prover.model.entries

import net.prover.model.Inference.RearrangementType
import net.prover.model.expressions.Statement
import net.prover.model._

case class Axiom(
    name: String,
    key: String,
    chapterTitle: String,
    bookTitle: String,
    premises: Seq[Premise],
    conclusion: Statement,
    rearrangementType: RearrangementType = RearrangementType.NotRearrangement)
  extends ChapterEntry
    with Inference.Entry
{
  override def chapterKey = chapterTitle.formatAsKey
  override def bookKey = bookTitle.formatAsKey

  override def inferences: Seq[Inference] = Seq(this)
  override def serializedLines: Seq[String] = {
    Seq(s"axiom $name") ++
      rearrangementType.serialized.toSeq ++
      premises.map(_.serialized) ++
      Seq(s"conclusion ${conclusion.serialized}")
  }
}

object Axiom extends ChapterEntryParser {
  override val name: String = "axiom"

  private def conclusionParser(implicit context: ParsingContext): Parser[Statement] = {
    for {
      _ <- Parser.requiredWord("conclusion")
      conclusion <- Statement.parser
    } yield conclusion
  }

  def parser(chapterTitle: String, bookTitle: String, getKey: String => String)(implicit context: ParsingContext): Parser[Axiom] = {
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      premises <- Premise.listParser
      conclusion <- conclusionParser
    } yield {
      Axiom(
        name,
        getKey(name),
        chapterTitle,
        bookTitle,
        premises,
        conclusion,
        rearrangementType)
    }
  }
  override def toString = name
}
