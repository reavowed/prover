package net.prover.model.definitions

import net.prover.model._
import net.prover.model.entries.ChapterEntry
import net.prover.model.expressions.Term
import net.prover.model.proof.SubstitutionContext

case class TermListAdapter(templates: Seq[Term]) {
  def specifyTerms(arguments: Seq[Term]): Seq[Term] = {
    templates.map(_.specify(arguments)(SubstitutionContext.outsideProof).get)
  }

  def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): TermListAdapter = {
    TermListAdapter(templates.map(_.replaceDefinitions(expressionDefinitionReplacements)))
  }

  def referencedEntries: Set[ChapterEntry] = templates.flatMap(_.referencedDefinitions).map(_.associatedChapterEntry).toSet
  def serialized: String = templates.map(_.serialized).mkString(", ").inParens
}

object TermListAdapter {
  def parser(variableDefinitions: Seq[SimpleVariableDefinition])(implicit entryContext: EntryContext): Parser[TermListAdapter] = {
    val expressionParsingContext = ExpressionParsingContext.forTypeDefinition(variableDefinitions)
    for {
      templates <- Term.parser(expressionParsingContext).optionalListInParens(Some(","))
    } yield TermListAdapter(templates)
  }
}
