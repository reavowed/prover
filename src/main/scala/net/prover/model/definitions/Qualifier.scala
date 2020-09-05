package net.prover.model.definitions

import net.prover._
import net.prover.model._

case class Qualifier(variableDefinitions: Seq[SimpleVariableDefinition], format: Format.Explicit) {
  def prependFormat(formatToPrepend: Format): Format = Format.Concatenated(formatToPrepend, format)
  def withFormat(newFormat: Format.Explicit): Qualifier = {
    Qualifier(variableDefinitions, newFormat)
  }
  def serialized: String = variableDefinitions.map(_.serialized).mkString(" ").inParens + " " + format.serializedWithoutPrefix
}

object Qualifier {
  def parser: Parser[Qualifier] = for {
    variableDefinitions <- SimpleVariableDefinition.listParser
    format <- Format.parserForTypeDefinition(variableDefinitions)
  } yield Qualifier(variableDefinitions, format)

  implicit class OptionOps(optionalQualifier: Option[Qualifier]) {
    def variableDefinitions: Seq[SimpleVariableDefinition] = optionalQualifier.map(_.variableDefinitions).getOrElse(Nil)
    def prependFormat(format: Format): Format = optionalQualifier.map(_.prependFormat(format)).getOrElse(format)
  }
}
