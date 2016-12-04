package net.prover.model

case class Predicate(symbol: String, arity: Int, definingStatement: Option[Statement]) extends ChapterEntry(Predicate) {
  val defaultStatement: PredicateStatement = apply((1 to arity).map(TermVariable): _*)

  def parseStatement(line: PartialLine, context: Context): (Statement, PartialLine) = {
    (1 to arity).mapFold(line) { case (_, lineSoFar) =>
        Term.parse(lineSoFar, context)
    }.mapLeft(apply(_: _*))
  }

  def apply(terms: Term*): PredicateStatement = {
    PredicateStatement(terms, this)
  }

  def definition: Option[Definition] = definingStatement.map { d => new Definition {
    override val id: String = "definition-" + symbol
    override def definedStatement: Statement = defaultStatement
    override def definingStatement: Statement = d
  }}
}

object Predicate extends SingleLineChapterEntryParser[Predicate] {
  override val name: String = "predicate"
  override def parse(line: PartialLine, context: Context): Predicate = {
    val (symbol, lineAfterSymbol) = line.splitFirstWord
    val (arity, lineAfterArity) = lineAfterSymbol.splitFirstWord match {
      case (IntParser(arity), lineAfterArity) =>
        (arity, lineAfterArity)
      case _ =>
        throw ParseException.withMessage("Predicate arity must be an integer", line.fullLine)
    }
    val definingStatementOption = Statement.parseOptional(lineAfterArity, context)._1
    Predicate(symbol, arity, definingStatementOption)
  }
  override def addToContext(predicate: Predicate, context: Context): Context = {
    context.copy(predicates = context.predicates :+ predicate)
  }
}
