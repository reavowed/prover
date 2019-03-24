package net.prover.model.entries

import net.prover.model.Inference.RearrangementType
import net.prover.model.expressions.Statement
import net.prover.model._

case class Axiom(
    name: String,
    override val key: ChapterEntry.Key.Standalone,
    premises: Seq[Premise],
    conclusion: Statement,
    rearrangementType: RearrangementType = RearrangementType.NotRearrangement)
  extends Inference.Entry
{
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

  def parser(getKey: String => (String, Chapter.Key))(implicit context: ParsingContext): Parser[Axiom] = {
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      premises <- Premise.listParser
      conclusion <- conclusionParser
    } yield {
      Axiom(
        name,
        (ChapterEntry.Key.Standalone.apply _).tupled(getKey(name)),
        premises,
        conclusion,
        rearrangementType)
    }
  }
  override def toString = name
}
