package net.prover.model.entries

import net.prover.model.Inference.RearrangementType
import net.prover.model.expressions.Statement
import net.prover.model._

case class Axiom(
    name: String,
    premises: Seq[Statement],
    conclusion: Statement,
    rearrangementType: RearrangementType = RearrangementType.NotRearrangement)
  extends Inference.Entry
{
  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = premises.flatMap(_.referencedDefinitions).toSet ++ conclusion.referencedDefinitions
  override def inferences: Seq[Inference] = Seq(this)
  override def serializedLines: Seq[String] = {
    Seq(s"axiom $name") ++
      rearrangementType.serialized.toSeq ++
      premises.map("premise " + _.serialized) ++
      Seq(s"conclusion ${conclusion.serialized}")
  }
}

object Axiom extends Inference.EntryParser {
  override val name: String = "axiom"

  def parser(implicit entryContext: EntryContext): Parser[Axiom] = {
    implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      premises <- premisesParser
      conclusion <- conclusionParser
    } yield {
      Axiom(
        name,
        premises,
        conclusion,
        rearrangementType)
    }
  }
  override def toString = name
}
