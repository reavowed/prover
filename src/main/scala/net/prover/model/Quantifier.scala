package net.prover.model

case class Quantifier(symbol: String) extends ChapterEntry {
  override val `type`: String = "quantifier"
  val defaultStatement: Statement = apply(TermVariable(1), Atom(1))

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
}

object Quantifier extends SingleLineChapterEntryParser[Quantifier] {
  override val name: String = "quantifier"
  override def parse(line: PartialLine, context: Context): Quantifier = {
    val (symbol, _) = line.splitFirstWord
    Quantifier(symbol)
  }
  override def addToBook(quantifier: Quantifier, book: Book): Book = {
    book.copy(quantifiers = book.quantifiers :+ quantifier)
  }
}
