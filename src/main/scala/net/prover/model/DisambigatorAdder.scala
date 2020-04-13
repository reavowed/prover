package net.prover.model

import net.prover.model.entries.ExpressionDefinition
import net.prover.model.expressions.{Template, Term}

case class DisambigatorAdder(template: Template, disambiguator: String) {
  def replaceDefinition(
    oldDefinition: ExpressionDefinition,
    newDefinition: ExpressionDefinition
  ): DisambigatorAdder = {
    DisambigatorAdder(template.replaceDefinition(oldDefinition, newDefinition), disambiguator)
  }
  def serialized: String = template.serialized + " " + disambiguator
}

object DisambigatorAdder {
  def parser(implicit entryContext: EntryContext): Parser[DisambigatorAdder] = {
    for {
      template <- Term.templateParser(TemplateParsingContext(entryContext, Seq(Seq("_" -> 0))))
      disambiguator <- Parser.singleWord
    } yield DisambigatorAdder(template, disambiguator)
  }
  def listParser(implicit entryContext: EntryContext): Parser[Seq[DisambigatorAdder]] = {
    parser.listInParens(Some(","))
  }

  implicit class ListOps(list: Seq[DisambigatorAdder]) {
    def serialized: String = list.map(_.serialized).mkString(", ").inParens
  }
}
