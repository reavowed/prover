package net.prover.model.entries

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.expressions.Statement
import net.prover.model.proof.Proof

@JsonIgnoreProperties(Array("rearrangementType", "allowsRearrangement", "proofOutline"))
case class Theorem(
    name: String,
    key: ChapterEntry.Key.Standalone,
    premises: Seq[Premise],
    conclusion: Statement,
    proof: Proof,
    rearrangementType: RearrangementType)
  extends Inference.Entry
{
  def referencedInferenceIds: Set[String] = proof.referencedInferenceIds
  override def inferences: Seq[Inference] = Seq(this)

  override def serializedLines = Seq(s"theorem $name") ++
    rearrangementType.serialized.toSeq ++
    premises.map(_.serialized) ++
    Seq("conclusion " + conclusion.serialized) ++
    proof.serializedLines

  override def toString = name
}

object Theorem extends ChapterEntryParser {
  override val name: String = "theorem"

  private def conclusionParser(implicit context: ParsingContext): Parser[Statement] = {
    for {
      _ <- Parser.requiredWord("conclusion")
      conclusion <- Statement.parser
    } yield conclusion
  }

  def parser(getKey: String => (String, Chapter.Key))(implicit context: ParsingContext): Parser[Theorem] = {
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      premises <- Premise.listParser
      conclusion <- conclusionParser
      proof <- Proof.parser
    } yield {
      Theorem(
        name,
        (ChapterEntry.Key.Standalone.apply _).tupled(getKey(name)),
        premises,
        conclusion,
        proof,
        rearrangementType)
    }
  }
}
