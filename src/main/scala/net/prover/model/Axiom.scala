package net.prover.model

case class Axiom(id: String, title: String, statement: Statement) extends ChapterEntry with DirectStepParser {
  override val `type`: String = "axiom"

  override def readStep(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context): (Step, PartialLine) = {
    val (matcher, remainingLine) = Match.empty.expand(statement.freeVariables, line, context)
    (Step(statement.applyMatch(matcher)), remainingLine)
  }
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
    context.copy(axioms = context.axioms :+ axiom)
  }
}
