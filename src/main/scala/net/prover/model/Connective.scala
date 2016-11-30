package net.prover.model

case class Connective(name: String, symbol: String, arity: Int, definingStatement: Option[Statement]) extends ChapterEntry {
  val `type` = "connective"
  val defaultStatement: ConnectiveStatement = apply((1 to arity).map(StatementVariable): _*)

  def parseStatement(line: PartialLine, context: Context): (Statement, PartialLine) = {
    (1 to arity).mapFold(line) { case (_, lineSoFar) =>
      Statement.parse(lineSoFar, context)
    }.mapLeft(apply(_: _*))
  }

  def apply(substatements: Statement*): ConnectiveStatement = {
    ConnectiveStatement(substatements, this)
  }

  def definition: Option[Definition] = definingStatement.map { d => new Definition {
    override val name: String = "definition-" + Connective.this.name
    override def definedStatement: ConnectiveStatement = defaultStatement
    override def definingStatement: Statement = d
  }}
}

object Connective extends SingleLineChapterEntryParser[Connective] {
  override val name: String = "connective"
  override def parse(line: PartialLine, context: Context): Connective = {
    val (name, lineAfterName) = line.splitFirstWord
    val (symbol, lineAfterSymbol) = lineAfterName.splitFirstWord
    val (arity, lineAfterArity) = lineAfterSymbol.splitFirstWord match {
      case (IntParser(arity), lineAfterArity) =>
        (arity, lineAfterArity)
      case _ =>
        throw ParseException.withMessage("Connective arity must be an integer", line.fullLine)
    }
    val definingStatementOption = if (lineAfterArity.nonEmpty) {
      Some(Statement.parse(lineAfterArity, context)._1)
    } else {
      None
    }
    Connective(name, symbol, arity, definingStatementOption)
  }
  override def addToContext(connective: Connective, context: Context): Context = {
    context.copy(connectives = context.connectives :+ connective)
  }
}
