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

object Connective {
  def parse(definitionText: String): Connective = {
    definitionText.splitByWhitespace() match {
      case Seq(name, symbol, IntParser(arity)) =>
        Connective(name, symbol, arity)
      case _ =>
        throw new Exception("Could not parse statement definition\n" + definitionText)
    }
  }
}
