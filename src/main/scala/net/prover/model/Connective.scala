package net.prover.model

case class Connective(name: String, symbol: String, arity: Int) extends ChapterEntry {
  val `type` = "connective"
  val defaultStatement: ConnectiveStatement = apply((1 to arity).map(Atom): _*)

  def parseStatement(line: PartialLine, context: Context): (Statement, PartialLine) = {
    (1 to arity).foldLeft((Seq.empty[Statement], line)) {
      case ((statements, lineSoFar), _) =>
        Statement.parse(lineSoFar, context).mapLeft(statements :+ _)
    }.mapLeft(this.apply(_: _*))
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
  override def addToBook(connective: Connective, book: Book): Book = {
    book.copy(connectives = book.connectives :+ connective)
  }
}
