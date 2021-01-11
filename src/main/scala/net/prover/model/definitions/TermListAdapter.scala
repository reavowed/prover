package net.prover.model.definitions

import net.prover.core.expressions.Term
import net.prover.model._
import net.prover.structure.EntryContext
import net.prover.structure.model.entries.ChapterEntry
import net.prover.structure.parsers.TermParsers

case class TermListAdapter(variableDefinitions: Seq[SimpleVariableDefinition], templates: Seq[Term]) {
  def replaceDefinitions(expressionDefinitionReplacements: Map[CompoundExpressionDefinition, CompoundExpressionDefinition]): TermListAdapter = {
    TermListAdapter(variableDefinitions, templates.map(_.replaceDefinitions(expressionDefinitionReplacements)))
  }

  def referencedEntries: Set[ChapterEntry] = templates.flatMap(_.referencedDefinitions).map(_.associatedChapterEntry).toSet
  def serialized: String = variableDefinitions.serialized + " " + templates.map(_.serialized).mkString(", ").inParens
}

object TermListAdapter {
  def parser(implicit entryContext: EntryContext): Parser[TermListAdapter] = {
    for {
      variableDefinitions <- SimpleVariableDefinition.listParser
      expressionParsingContext = ExpressionParsingContext.forTypeDefinition(variableDefinitions)
      templates <- TermParsers.termParser(expressionParsingContext).listInParens(Some(","))
    } yield TermListAdapter(variableDefinitions, templates)
  }
}
