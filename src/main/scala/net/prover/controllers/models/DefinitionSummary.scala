package net.prover.controllers.models

import net.prover.model._
import net.prover.model.entries.{ExpressionDefinitionEntry, TermDefinitionEntry}
import net.prover.model.expressions.{DefinedTermTemplate, Template}

case class DisambiguatorAdderSummary(template: Template, disambiguator: String)

case class DefinitionSummary(
  symbol: DisambiguatedSymbol,
  baseFormatString: String,
  requiresBrackets: Boolean,
  requiresComponentBrackets: Boolean,
  numberOfBoundVariables: Int,
  numberOfComponents: Int,
  attributes: Seq[String],
  disambiguatorAdders: Seq[DisambiguatorAdderSummary])

object DefinitionSummary {
  def getAllFromContext(entryContext: EntryContext): Map[String, DefinitionSummary] = {
    entryContext.availableEntries.ofType[ExpressionDefinitionEntry]
      .map(d =>
        d.disambiguatedSymbol.serialized -> DefinitionSummary(
          d.disambiguatedSymbol,
          d.format.baseFormatString,
          d.format.requiresBrackets,
          d.format.requiresComponentBrackets,
          d.boundVariableNames.length,
          d.componentTypes.length,
          d.attributes,
          getDisambiguatorAdderSummaries(d)))
      .toMap
  }
  def getDisambiguatorAdderSummaries(expressionDefinition: ExpressionDefinitionEntry): Seq[DisambiguatorAdderSummary] = expressionDefinition match {
    case termDefinition: TermDefinitionEntry => {
      termDefinition.disambiguatorAdders.map { da =>
        DisambiguatorAdderSummary(da.template.specify(Seq(DefinedTermTemplate(termDefinition, Nil, Nil))), da.disambiguator)
      }
    }
    case _ => Nil
  }
}
