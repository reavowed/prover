package net.prover.model.entries

import net.prover.model.Inference.RearrangementType
import net.prover.model.expressions.Statement
import net.prover.model._

case class Axiom(
    name: String,
    override val key: ChapterEntry.Key.Standalone,
    premises: Seq[Statement],
    conclusion: Statement,
    rearrangementType: RearrangementType = RearrangementType.NotRearrangement)
  extends Inference.Entry
{
  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedDefinitions: Set[ExpressionDefinition] = premises.flatMap(_.referencedDefinitions).toSet ++ conclusion.referencedDefinitions
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

  def parser(getKey: String => (String, Chapter.Key))(implicit context: ParsingContext): Parser[Axiom] = {
    for {
      name <- Parser.toEndOfLine
      rearrangementType <- RearrangementType.parser
      premises <- premisesParser
      conclusion <- conclusionParser
    } yield {
      Axiom(
        name,
        ChapterEntry.Key.Standalone(name, getKey),
        premises,
        conclusion,
        rearrangementType)
    }
  }
  override def toString = name
}
