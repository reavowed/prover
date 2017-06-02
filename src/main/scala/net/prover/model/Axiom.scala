package net.prover.model

import net.prover.model.Inference.Premise

case class Axiom(
    name: String,
    premises: Seq[Premise],
    conclusion: ProvenStatement)
  extends ChapterEntry(Axiom)
  with Inference {

  val id = calculateHash()
}

object Axiom extends ChapterEntryParser[Axiom] with InferenceParser {
  override val name: String = "axiom"

  private def conclusionParser(implicit context: Context): Parser[Statement] = {
    for {
      _ <- Parser.requiredWord("conclusion")
      conclusion <- Statement.parser
    } yield conclusion
  }

  def parser(implicit context: Context): Parser[Axiom] = {
    for {
      name <- Parser.toEndOfLine
      premises <- premisesParser
      conclusion <- conclusionParser
      conditions <- Conditions.parser
    } yield {
      Axiom(name, premises, ProvenStatement(conclusion, conditions))
    }
  }

  override def addToContext(axiom: Axiom, context: Context): Context = {
    context.copy(inferences = context.inferences :+ axiom)
  }
}
