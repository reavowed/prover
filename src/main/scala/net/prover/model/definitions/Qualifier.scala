package net.prover.model.definitions

import net.prover.model._

case class Qualifier(termNames: Seq[String], format: Format.Explicit) {
  def prependFormat(formatToPrepend: Format): Format = Format.Concatenated(formatToPrepend, format)
  def withFormat(newFormat: Format.Explicit): Qualifier = {
    Qualifier(termNames, newFormat)
  }
  def serialized: String = termNames.mkString(" ").inParens + " " + format.serializedWithoutPrefix
}

object Qualifier {
  def parser: Parser[Qualifier] = for {
    termNames <- Parser.singleWord.listInParens(None)
    format <- Format.parser(termNames)
  } yield Qualifier(termNames, format)

  implicit class OptionOps(optionalQualifier: Option[Qualifier]) {
    def termNames: Seq[String] = optionalQualifier.map(_.termNames).getOrElse(Nil)
    def prependFormat(format: Format): Format = optionalQualifier.map(_.prependFormat(format)).getOrElse(format)
  }
}
