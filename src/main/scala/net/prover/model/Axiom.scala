package net.prover.model

case class Axiom(
    id: String,
    title: String,
    assumption: Option[Statement],
    premises: Seq[Statement],
    conclusion: Statement,
    arbitraryVariables: Seq[TermVariable],
    distinctVariables: DistinctVariables)
  extends ChapterEntry(Axiom)
    with Inference


object Axiom extends ChapterEntryParser[Axiom] {
  override val name: String = "axiom"

  def parser(implicit context: Context): Parser[Axiom] = {
    for {
      id <- Parser.singleWord
      title <- Parser.allInParens
      assumption <- Statement.parser.optionalInParens
      premises <- Statement.listParser
      conclusion <- Statement.parser.inParens
      arbitraryVariables <- Term.variableListParser
      distinctVariables <- DistinctVariables.parser
    } yield {
      Axiom(id, title, assumption, premises, conclusion, arbitraryVariables, distinctVariables)
    }
  }

  override def addToContext(axiom: Axiom, context: Context): Context = {
    context.copy(theoremLineParsers = context.theoremLineParsers :+ axiom)
  }
}
