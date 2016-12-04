package net.prover.model

case class Quantifier(symbol: String, definingStatement: Option[Statement]) extends ChapterEntry(Quantifier) {
  val defaultStatement: Statement = apply(TermVariable(1), StatementVariable(1))

  def parseStatement(line: PartialLine, context: Context): (Statement, PartialLine) = {
    val (term, lineAterTerm) = Term.parse(line, context)
    val termVariable = term match {
      case t: TermVariable =>
        t
      case _ =>
        throw ParseException.withMessage("Quantifier term must be a variable", line.fullLine)
    }
    val (innerStatement, lineAfterInnerStatement) = Statement.parse(lineAterTerm, context)
    (apply(termVariable, innerStatement), lineAfterInnerStatement)
  }

  def apply(term: TermVariable, quantifiedStatement: Statement): QuantifierStatement = {
    QuantifierStatement(term, quantifiedStatement, this)
  }

  def definition: Option[Definition] = definingStatement.map { d => new Definition {
    override val id: String = "definition-" + symbol
    override def definedStatement: Statement = defaultStatement
    override def definingStatement: Statement = d
  }}
}

object Quantifier extends SingleLineChapterEntryParser[Quantifier] {
  override val name: String = "quantifier"
  override def parse(line: PartialLine, context: Context): Quantifier = {
    val (symbol, lineAfterSymbol) = line.splitFirstWord
    val definingStatementOption = Statement.parseOptional(lineAfterSymbol, context)._1
    Quantifier(symbol, definingStatementOption)
  }
  override def addToContext(quantifier: Quantifier, context: Context): Context = {
    context.copy(quantifiers = context.quantifiers :+ quantifier)
  }
}
