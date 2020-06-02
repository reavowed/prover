package net.prover.model.definitions

import net.prover.model._

case class Qualifier(defaultTermNames: Seq[String], format: Format.Explicit) {
  def prependFormat(formatToPrepend: Format): Format = Format.Concatenated(formatToPrepend, format)
  def withFormat(newFormat: Format.Explicit): Qualifier = {
    Qualifier(defaultTermNames, newFormat)
  }
  def serialized: String = defaultTermNames.mkString(" ").inParens + " " + format.serializedWithoutPrefix
}

object Qualifier {
  def parser: Parser[Qualifier] = for {
    defaultTermNames <- Parser.singleWord.listInParens(None)
    format <- Format.parser(defaultTermNames)
  } yield Qualifier(defaultTermNames, format)

  implicit class OptionOps(optionalQualifier: Option[Qualifier]) {
    def defaultTermNames: Seq[String] = optionalQualifier.map(_.defaultTermNames).getOrElse(Nil)
    def prependFormat(format: Format): Format = optionalQualifier.map(_.prependFormat(format)).getOrElse(format)
  }
}
