package net.prover.model

case class Constant(symbol: String, definition: Option[Statement]) extends ChapterEntry(Constant) with TermParser {
  override def parseTerm(line: PartialLine, context: Context): (Term, PartialLine) = {
    (ConstantTerm(symbol), line)
  }
}

object Constant extends SingleLineChapterEntryParser[Constant] {
  override val name: String = "constant"
  override def parse(line: PartialLine, context: Context): Constant = {
    val (symbol, lineAfterSymbol) = line.splitFirstWord
    val (definitionTemplate, _) = Statement.parse(lineAfterSymbol, addToContext(Constant(symbol, None), context))
    Constant(symbol, Some(definitionTemplate))
  }
  override def addToContext(constant: Constant, context: Context): Context = {
    context.copy(constants = context.constants :+ constant)
  }
}

