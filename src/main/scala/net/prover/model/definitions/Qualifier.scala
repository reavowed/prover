package net.prover.model.definitions

import net.prover.model._

case class Qualifier(termNames: Seq[String], format: Format.Explicit) {
  def withFormat(newFormat: Format.Explicit): Qualifier = {
    Qualifier(termNames, newFormat)
  }
  def serialized: String = termNames.mkString(" ").inParens + " " + format.serializedWithoutPrefix

  def childTermNamesParser: Parser[Seq[String]] = {
    termNames.map(_ => Parser.singleWord).traverseParser
  }
}

object Qualifier {
  def parser: Parser[Qualifier] = for {
    termNames <- Parser.singleWord.listInParens(None)
    format <- Format.parser(termNames)
  } yield Qualifier(termNames, format)
}
