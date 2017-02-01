package net.prover.model

case class Axiom(id: String, title: String, conclusion: Statement) extends ChapterEntry(Axiom) with Inference {
  val premises: Seq[Statement] = Nil
  val arbitraryVariables: Seq[TermVariable] = Nil
  val distinctVariables: DistinctVariables = DistinctVariables.empty
}

object Axiom extends SingleLineChapterEntryParser[Axiom] {
  override val name: String = "axiom"

  def parser(context: Context): Parser[Axiom] = {
    for {
      id <- Parser.singleWord
      title <- Parser.allInParens
      statement <- Statement.parser(context).inParens
    } yield {
      Axiom(id, title, statement)
    }
  }

  override def parse(line: PartialLine, context: Context): Axiom = {
    parser(context).parse(line)._1
  }

  override def addToContext(axiom: Axiom, context: Context): Context = {
    context.copy(theoremLineParsers = context.theoremLineParsers :+ axiom)
  }
}
