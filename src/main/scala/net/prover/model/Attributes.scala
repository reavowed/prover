package net.prover.model

object Attributes {
  def parser: Parser[Seq[String]] = {
    Parser.optional("attributes", Parser.wordsInParens, Nil)
  }

  def serialize(attributes: Seq[String]): Option[String] = {
    if (attributes.nonEmpty) {
      Some(s"attributes (${attributes.mkString(" ")})")
    } else {
      None
    }
  }
}
