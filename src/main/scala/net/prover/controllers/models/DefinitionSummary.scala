package net.prover.controllers.models

import net.prover.model._
import net.prover.model.entries.ExpressionDefinition

case class DefinitionSummary(
  symbol: DisambiguatedSymbol,
  baseFormatString: String,
  requiresBrackets: Boolean,
  requiresComponentBrackets: Boolean,
  numberOfBoundVariables: Int,
  numberOfComponents: Int,
  attributes: Seq[String])

object DefinitionSummary {
  def getAllFromContext(entryContext: EntryContext): Map[String, DefinitionSummary] = {
    entryContext.availableEntries.ofType[ExpressionDefinition]
      .map(d => d.disambiguatedSymbol.serialized -> DefinitionSummary(d.disambiguatedSymbol, d.format.baseFormatString, d.format.requiresBrackets, d.format.requiresComponentBrackets, d.boundVariableNames.length, d.componentTypes.length, d.attributes))
      .toMap
  }
}
