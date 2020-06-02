package net.prover.model.definitions

import net.prover.model.expressions.Term
import net.prover.model._

case class TermListAdapter(variableDefinitions: Seq[SimpleVariableDefinition], templates: Seq[Term]) {
  def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): TermListAdapter = {
    TermListAdapter(variableDefinitions, templates.map(_.replaceDefinitions(expressionDefinitionReplacements)))
  }

  def serialized: String = variableDefinitions.serialized + " " + templates.map(_.serialized).mkString(", ").inParens
}

object TermListAdapter {
  def parser(implicit entryContext: EntryContext): Parser[TermListAdapter] = {
    for {
      variableDefinitions <- SimpleVariableDefinition.listParser
      expressionParsingContext = ExpressionParsingContext.forTypeDefinition(variableDefinitions)
      templates <- Term.parser(expressionParsingContext).listInParens(Some(","))
    } yield TermListAdapter(variableDefinitions, templates)
  }
}
