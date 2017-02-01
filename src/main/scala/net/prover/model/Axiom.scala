package net.prover.model

case class Axiom(id: String, title: String, conclusion: Statement) extends ChapterEntry(Axiom) with Inference {
  val premises: Seq[Statement] = Nil
  val arbitraryVariables: Seq[TermVariable] = Nil
  val distinctVariables: DistinctVariables = DistinctVariables.empty
}

object Axiom extends SingleLineChapterEntryParser[Axiom] {
  override val name: String = "axiom"
  override def parse(line: PartialLine, context: Context): Axiom = {
    val (id, lineAfterId) = line.splitFirstWord
    val (title, lineAfterTitle) = lineAfterId.splitFirstWord
    val (statement, _) = Statement.parse(lineAfterTitle, context)
    Axiom(id, title, statement)
  }
  override def addToContext(axiom: Axiom, context: Context): Context = {
    context.copy(theoremLineParsers = context.theoremLineParsers :+ axiom)
  }
}
