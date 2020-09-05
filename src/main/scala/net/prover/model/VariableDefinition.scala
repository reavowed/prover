package net.prover.model

import net.prover._

case class VariableDefinition(name: String, arity: Int, attributes: Seq[String]) {
  def serialized: String = (Seq(name, arity.toString) ++ attributes.optionalListInParens(", ").toSeq).mkString(" ")
}

object VariableDefinition {
  def parser: Parser[VariableDefinition] = for {
    name <- Parser.singleWord
    arity <- Parser.int
    attributes <- Parser.singleWord.optionalListInParens(None)
  } yield VariableDefinition(name, arity, attributes)
  def listParser: Parser[Seq[VariableDefinition]] = parser.listInParens(Some(","))
}

case class SimpleVariableDefinition(name: String, attributes: Seq[String]) {
  def toFullVariableDefinition: VariableDefinition = VariableDefinition(name, 0, attributes)
  def serialized: String = (name +: attributes.optionalListInParens(" ").toSeq).mkString(" ")
}

object SimpleVariableDefinition {
  def parser: Parser[SimpleVariableDefinition] = for {
    name <- Parser.singleWord
    attributes <- Parser.singleWord.optionalListInParens(None)
  } yield SimpleVariableDefinition(name, attributes)
  def listParser: Parser[Seq[SimpleVariableDefinition]] = parser.listInParens(None)

  implicit class SeqOps(seq: Seq[SimpleVariableDefinition]) {
    def serialized: String = seq.map(_.serialized).mkString(" ").inParens
  }
}
