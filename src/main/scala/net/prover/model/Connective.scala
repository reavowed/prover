package net.prover.model

case class Connective(name: String, symbol: String, arity: Int) extends ChapterEntry {
  val `type` = "connective"
  val defaultStatement: ConnectiveStatement = apply((1 to arity).map(Atom): _*)

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
    line.splitWords match {
      case Seq(connectiveName, symbol, IntParser(arity)) =>
        Connective(connectiveName, symbol, arity)
      case _ =>
        throw ParseException.withMessage("Could not parse connective definition", line.fullLine)
    }
  }
  override def addToContext(connective: Connective, context: Context): Context = {
    context.copy(connectives = context.connectives :+ connective)
  }
}
