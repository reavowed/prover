package net.prover.model.components

import net.prover.model.{Context, Parser}

trait ComponentType {
  def parser(implicit context: Context): Parser[Component]
}

object ComponentType {
  private val componentTypesByName = Map(
    "term" -> Term,
    "statement" -> Statement)

  def parser: Parser[ComponentType] = {
    for {
      name <- Parser.singleWord
    } yield {
      componentTypesByName.getOrElse(
        name,
        throw new Exception(s"Unrecognised statement type $name"))
    }
  }

  def listParser: Parser[Seq[ComponentType]] = parser.listInParens(None)

  implicit class ComponentTypeSeqOps(componentTypes: Seq[ComponentType]) {
    def componentsParser(implicit context: Context) = {
      componentTypes.map(_.parser).traverseParser
    }
  }
}
