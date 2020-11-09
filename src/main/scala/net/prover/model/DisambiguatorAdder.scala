package net.prover.model

import net.prover._
import net.prover.model.definitions.ExpressionDefinition
import net.prover.model.expressions.{Template, Term}
import net.prover.structure.EntryContext

case class DisambiguatorAdder(template: Template, disambiguator: String) {
  def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): DisambiguatorAdder = {
    DisambiguatorAdder(template.replaceDefinitions(expressionDefinitionReplacements), disambiguator)
  }
  def serialized: String = template.serialized + " " + disambiguator
}

object DisambiguatorAdder {
  def parser(implicit entryContext: EntryContext): Parser[DisambiguatorAdder] = {
    for {
      template <- Term.templateParser(TemplateParsingContext(entryContext, Seq(Seq("_" -> 0))))
      disambiguator <- Parser.singleWord
    } yield DisambiguatorAdder(template, disambiguator)
  }
  def listParser(implicit entryContext: EntryContext): Parser[Seq[DisambiguatorAdder]] = {
    parser.listInParens(Some(","))
  }

  implicit class ListOps(list: Seq[DisambiguatorAdder]) {
    def serialized: String = list.map(_.serialized).mkString(", ").inParens
  }
}
