package net.prover.model

case class Predicate(
    symbol: String,
    arity: Int,
    definingStatement: Option[Statement])
  extends ChapterEntry(Predicate) with StatementDefinition {
  val defaultStatement: PredicateStatement = apply((1 to arity).map(i => TermVariable((123 - i).toChar.toString)): _*)
  val distinctVariables: DistinctVariables = DistinctVariables.empty

  def parseStatement(line: PartialLine, context: Context): (Statement, PartialLine) = {
    (1 to arity).mapFold(line) { case (_, lineSoFar) =>
        Term.parse(lineSoFar, context)
    }.mapLeft(apply(_: _*))
  }

  def apply(terms: Term*): PredicateStatement = {
    PredicateStatement(terms, this)
  }
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
    context.copy(statementDefinitions = context.statementDefinitions :+ predicate)
  }
}
