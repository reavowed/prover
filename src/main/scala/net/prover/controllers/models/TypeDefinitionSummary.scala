package net.prover.controllers.models

import net.prover.model.EntryContext

case class TypeDefinitionSummary(symbol: String, name: String, numberOfComponents: Int, componentFormatString: String, article: String, properties: Seq[PropertyDefinitionSummary])
case class PropertyDefinitionSummary(symbol: String, qualifiedSymbol: String, name: String)

object TypeDefinitionSummary {
  def getAllFromContext(entryContext: EntryContext): Map[String, TypeDefinitionSummary] = {
    entryContext.typeDefinitions
      .map(d => d.symbol -> TypeDefinitionSummary(
        d.symbol,
        d.name,
        d.otherComponentTypes.length,
        d.componentFormat.baseFormatString,
        d.article,
        entryContext.propertyDefinitionsByType.getOrElse(d.symbol, Nil).map(pd => PropertyDefinitionSummary(pd.symbol, pd.qualifiedSymbol, pd.name))))
      .toMap
  }
}
