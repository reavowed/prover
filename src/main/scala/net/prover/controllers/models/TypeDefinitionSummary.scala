package net.prover.controllers.models

import net.prover.model.{EntryContext, SimpleVariableDefinition}
import net.prover.model.definitions.Qualifier
import net.prover.model.entries.{PropertyDefinitionOnType, TypeDefinition}

case class TypeDefinitionSummary(
  symbol: String,
  name: String,
  article: String,
  defaultQualifier: Option[QualifierSummary],
  properties: Seq[PropertyDefinitionSummary],
  qualifiers: Seq[TypeQualifierDefinitionSummary],
  relatedObjects: Seq[RelatedObjectDefinitionSummary])
case class PropertyDefinitionSummary(symbol: String, qualifiedSymbol: String, name: String, requiredParentQualifier: Option[String], requiredParentObjects: Seq[String])
case class TypeQualifierDefinitionSummary(symbol: String, qualifiedSymbol: String, name: String, qualifier: QualifierSummary)
case class RelatedObjectDefinitionSummary(symbol: String, qualifiedSymbol: String, name: String, article: String, mainVariableDefinition: SimpleVariableDefinition, requiredParentQualifier: Option[String])
case class StandalonePropertyDefinitionSummary(symbol: String, qualifiedSymbol: String, name: String, mainVariableDefinition: SimpleVariableDefinition)
case class QualifierSummary(format: String, variableDefinitions: Seq[SimpleVariableDefinition])
case class TypeRelationDefinitionSummary(symbol: String, linkingPhrase: String)

object TypeDefinitionSummary {
  def getSummary(typeDefinition: TypeDefinition)(implicit entryContext: EntryContext): TypeDefinitionSummary = {
    TypeDefinitionSummary(
      typeDefinition.symbol,
      typeDefinition.name,
      typeDefinition.article,
      typeDefinition.defaultQualifier.map(getQualifierSummary),
      entryContext.propertyDefinitionsByType.getOrElse(typeDefinition.symbol, Nil).map(getPropertyDefinitionSummary),
      entryContext.qualifiersByType.getOrElse(typeDefinition.symbol, Nil).map(qd => TypeQualifierDefinitionSummary(qd.symbol, qd.qualifiedSymbol, qd.name, getQualifierSummary(qd.qualifier))),
      entryContext.relatedObjectsByType.getOrElse(typeDefinition.symbol, Nil).map(rod => RelatedObjectDefinitionSummary(rod.symbol, rod.qualifiedSymbol, rod.name, rod.article, rod.mainVariableDefinition, rod.parentTypeConditions.requiredParentQualifier.map(_.symbol))))
  }
  def getQualifierSummary(qualifier: Qualifier): QualifierSummary = {
    QualifierSummary(qualifier.format.baseFormatString, qualifier.variableDefinitions)
  }
  def getPropertyDefinitionSummary(pd: PropertyDefinitionOnType): PropertyDefinitionSummary = {
    PropertyDefinitionSummary(pd.symbol, pd.qualifiedSymbol, pd.name, pd.parentTypeConditions.requiredParentQualifier.map(_.symbol), pd.parentTypeConditions.requiredParentObjects.objectDefinitions.map(_.symbol))
  }
  def getAllFromContext(entryContext: EntryContext): Map[String, TypeDefinitionSummary] = {
    entryContext.typeDefinitions.mapValues(d => getSummary(d)(entryContext))
  }
}

object StandalonePropertyDefinitionSummary {
  def getAllFromContext(entryContext: EntryContext): Map[String, StandalonePropertyDefinitionSummary] = {
    entryContext.standalonePropertyDefinitions
      .map(d => d.symbol -> StandalonePropertyDefinitionSummary(
        d.symbol,
        d.qualifiedSymbol,
        d.name,
        d.mainVariableDefinition))
      .toMap
  }
}

object TypeRelationDefinitionSummary {
  def getAllFromContext(entryContext: EntryContext): Map[String, TypeRelationDefinitionSummary] = {
    entryContext.typeRelationDefinitions.map(d => d.symbol -> TypeRelationDefinitionSummary(d.symbol, d.linkingPhrase)).toMap
  }
}
