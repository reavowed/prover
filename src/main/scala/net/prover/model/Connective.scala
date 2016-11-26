package net.prover.model

case class Connective(name: String, symbol: String, arity: Int) {
  def parseStatement(fullText: String, connectives: Seq[Connective]): (Statement, String) = {
    (1 to arity).foldLeft((Seq.empty[Statement], fullText)) {
      case ((statements, textSoFar), _) =>
        Statement.parse(textSoFar, connectives).mapLeft(statements :+ _)
    }.mapLeft(this.apply(_: _*))
  }

  def apply(substatements: Statement*) = {
    ConnectiveStatement(substatements, this)
  }
}

object Connective extends SingleLineBookEntryParser[Connective] {
  override val name: String = "connective"
  override def parse(text: String, book: Book): Connective = {
    text.splitByWhitespace() match {
      case Seq(name, symbol, IntParser(arity)) =>
        Connective(name, symbol, arity)
      case _ =>
        throw new Exception("Could not parse connective definition\n" + text)
    }
  }
  override def addToBook(connective: Connective, book: Book): Book = {
    book.copy(connectives = book.connectives :+ connective)
  }
}
