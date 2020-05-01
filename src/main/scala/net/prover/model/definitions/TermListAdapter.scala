package net.prover.model.definitions

import net.prover.model.expressions.Term
import net.prover.model._

case class TermListAdapter(termNames: Seq[String], templates: Seq[Term]) {
  def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): TermListAdapter = {
    TermListAdapter(termNames, templates.map(_.replaceDefinitions(expressionDefinitionReplacements)))
  }

  def serialized: String = termNames.mkString(" ").inParens + " " + templates.map(_.serialized).mkString(", ").inParens
}

object TermListAdapter {
  def parser(implicit entryContext: EntryContext): Parser[TermListAdapter] = {
    for {
      termNames <- Parser.wordsInParens
      expressionParsingContext = ExpressionParsingContext.outsideProof(entryContext, Nil).addInitialParameters(termNames)
      templates <- Term.parser(expressionParsingContext).listInParens(Some(","))
    } yield TermListAdapter(termNames, templates)
  }
}
