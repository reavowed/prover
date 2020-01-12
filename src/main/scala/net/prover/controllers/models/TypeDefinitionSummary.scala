package net.prover.controllers.models

import net.prover.model.EntryContext

case class TypeDefinitionSummary(symbol: String, name: String, componentFormatString: String, article: String, properties: Map[String, String])

object TypeDefinitionSummary {
  def getAllFromContext(entryContext: EntryContext): Map[String, TypeDefinitionSummary] = {
    entryContext.typeDefinitions
      .map(d => d.symbol -> TypeDefinitionSummary(
        d.symbol,
        d.name,
        d.componentFormat.baseFormatString,
        d.article,
        entryContext.propertyDefinitionsByType.getOrElse(d.symbol, Nil).map(pd => pd.qualifiedSymbol -> pd.name).toMap))
      .toMap
  }
}
