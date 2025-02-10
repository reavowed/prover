package net.prover.model

import net.prover.parsing.Parser

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
