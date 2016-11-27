package net.prover.model

case class Connective(name: String, symbol: String, arity: Int) {
  def parseStatement(line: PartialLine, connectives: Seq[Connective]): (Statement, PartialLine) = {
    (1 to arity).foldLeft((Seq.empty[Statement], line)) {
      case ((statements, lineSoFar), _) =>
        Statement.parse(lineSoFar, connectives).mapLeft(statements :+ _)
    }.mapLeft(this.apply(_: _*))
  }

  def apply(substatements: Statement*) = {
    ConnectiveStatement(substatements, this)
  }
}

object Connective extends SingleLineBookEntryParser[Connective] {
  override val name: String = "connective"
  override def parse(line: PartialLine, book: Book): Connective = {
    line.splitWords match {
      case Seq(connectiveName, symbol, IntParser(arity)) =>
        Connective(connectiveName, symbol, arity)
      case _ =>
        throw ParseException.withMessage("Could not parse connective definition", line.fullLine)
    }
  }
  override def addToBook(connective: Connective, book: Book): Book = {
    book.copy(connectives = book.connectives :+ connective)
  }
}
