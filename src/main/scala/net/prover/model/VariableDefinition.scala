package net.prover.model

case class VariableDefinition(name: String, arity: Int, attributes: Seq[String]) {
  def serialized: String = (Seq(name, arity.toString) ++ Attributes.serialize(attributes).toSeq).mkString(" ")
}

object VariableDefinition {
  def parser: Parser[VariableDefinition] = for {
    name <- Parser.singleWord
    arity <- Parser.int
    attributes <- Attributes.parser
  } yield VariableDefinition(name, arity, attributes)
  def listParser: Parser[Seq[VariableDefinition]] = parser.listInParens(Some(","))
}




