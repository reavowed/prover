package net.prover.model

case class Connective(
    symbol: String,
    arity: Int,
    definingStatement: Option[Statement])
  extends ChapterEntry(Connective) with StatementDefinition
{
  val defaultStatement: ConnectiveStatement = apply((1 to arity).map(StatementVariable): _*)
  val distinctVariables: DistinctVariables = DistinctVariables.empty

  def parseStatement(line: PartialLine, context: Context): (Statement, PartialLine) = {
    (1 to arity).mapFold(line) { case (_, lineSoFar) =>
      Statement.parse(lineSoFar, context)
    }.mapLeft(apply(_: _*))
  }

  def apply(substatements: Statement*): ConnectiveStatement = {
    ConnectiveStatement(substatements, this)
  }
}

object Connective extends SingleLineChapterEntryParser[Connective] {
  override val name: String = "connective"
  override def parse(line: PartialLine, context: Context): Connective = {
    val (symbol, lineAfterSymbol) = line.splitFirstWord
    val (arity, lineAfterArity) = lineAfterSymbol.splitFirstWord match {
      case (IntParser(arity), lineAfterArity) =>
        (arity, lineAfterArity)
      case _ =>
        throw ParseException.withMessage("Connective arity must be an integer", line.fullLine)
    }
    val definingStatementOption = Statement.parseOptional(lineAfterArity, context)._1
    Connective(symbol, arity, definingStatementOption)
  }
  override def addToContext(connective: Connective, context: Context): Context = {
    context.copy(statementDefinitions = context.statementDefinitions :+ connective)
  }
}
