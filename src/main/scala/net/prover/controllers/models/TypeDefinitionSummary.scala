package net.prover.controllers.models

import net.prover.model.EntryContext
import net.prover.model.definitions.Qualifier

case class TypeDefinitionSummary(symbol: String, name: String, defaultTermName: String, defaultQualifier: Option[QualifierSummary], article: String, properties: Seq[PropertyDefinitionSummary], qualifiers: Seq[TypeQualifierDefinitionSummary])
case class PropertyDefinitionSummary(symbol: String, qualifiedSymbol: String, name: String, requiredParentQualifier: Option[String])
case class TypeQualifierDefinitionSummary(symbol: String, qualifiedSymbol: String, name: String, qualifier: QualifierSummary)
case class StandalonePropertyDefinitionSummary(symbol: String, qualifiedSymbol: String, name: String)
case class QualifierSummary(format: String, defaultTermNames: Seq[String], numberOfComponents: Int)

object TypeDefinitionSummary {
  def getQualifierSummary(qualifier: Qualifier): QualifierSummary = {
    QualifierSummary(qualifier.format.baseFormatString, qualifier.termNames, qualifier.termNames.length)
  }
  def getAllFromContext(entryContext: EntryContext): Map[String, TypeDefinitionSummary] = {
    entryContext.typeDefinitions
      .map(d => d.symbol -> TypeDefinitionSummary(
        d.symbol,
        d.name,
        d.defaultTermName,
        d.qualifier.map(getQualifierSummary),
        d.article,
        entryContext.propertyDefinitionsByType.getOrElse(d.symbol, Nil).map(pd => PropertyDefinitionSummary(pd.symbol, pd.qualifiedSymbol, pd.name, pd.requiredParentQualifier.map(_.symbol))),
        entryContext.qualifiersByType.getOrElse(d.symbol, Nil).map(qd => TypeQualifierDefinitionSummary(qd.symbol, qd.qualifiedSymbol, qd.name, getQualifierSummary(qd.qualifier)))))
      .toMap
  }
}

object StandalonePropertyDefinitionSummary {
  def getAllFromContext(entryContext: EntryContext): Map[String, StandalonePropertyDefinitionSummary] = {
    entryContext.standalonePropertyDefinitions
      .map(d => d.symbol -> StandalonePropertyDefinitionSummary(
        d.symbol,
        d.qualifiedSymbol,
        d.name))
      .toMap
  }
}
