package net.prover.model.components

import net.prover.model.{Parser, ParsingContext}

trait ComponentType {
  def parser(implicit context: ParsingContext): Parser[Component]
  def applicativeParser(implicit context: ParsingContext): Parser[Applicative[Component]]
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
    def componentsParser(implicit context: ParsingContext) = {
      componentTypes.map(_.parser).traverseParser
    }
    def applicativesParser(implicit context: ParsingContext) = {
      componentTypes.map(_.applicativeParser).traverseParser
    }
  }
}
