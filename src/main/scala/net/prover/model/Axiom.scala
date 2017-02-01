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


object Axiom extends SingleLineChapterEntryParser[Axiom] {
  override val name: String = "axiom"

  def parser(context: Context): Parser[Axiom] = {
    for {
      id <- Parser.singleWord
      title <- Parser.allInParens
      assumption <- Statement.parser(context).optionalInParens
      premises <- Statement.listParser(context)
      conclusion <- Statement.parser(context).inParens
      arbitraryVariables <- Term.variableListParser(context)
      distinctVariables <- DistinctVariables.parser(context)
    } yield {
      Axiom(id, title, assumption, premises, conclusion, arbitraryVariables, distinctVariables)
    }
  }

  override def parse(line: PartialLine, context: Context): Axiom = {
    parser(context).parse(line)._1
  }

  override def addToContext(axiom: Axiom, context: Context): Context = {
    context.copy(theoremLineParsers = context.theoremLineParsers :+ axiom)
  }
}
