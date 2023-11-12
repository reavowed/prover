package net.prover.controllers.models

import net.prover.model.{AvailableEntries, SimpleVariableDefinition}
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
  def getSummary(typeDefinition: TypeDefinition)(implicit availableEntries: AvailableEntries): TypeDefinitionSummary = {
    TypeDefinitionSummary(
      typeDefinition.symbol,
      typeDefinition.name,
      typeDefinition.article,
      typeDefinition.defaultQualifier.map(getQualifierSummary),
      availableEntries.propertyDefinitionsByType.getOrElse(typeDefinition.symbol, Nil).map(getPropertyDefinitionSummary),
      availableEntries.qualifiersByType.getOrElse(typeDefinition.symbol, Nil).map(qd => TypeQualifierDefinitionSummary(qd.symbol, qd.qualifiedSymbol, qd.name, getQualifierSummary(qd.qualifier))),
      availableEntries.relatedObjectsByType.getOrElse(typeDefinition.symbol, Nil).map(rod => RelatedObjectDefinitionSummary(rod.symbol, rod.qualifiedSymbol, rod.name, rod.article, rod.mainVariableDefinition, rod.parentTypeConditions.requiredParentQualifier.map(_.symbol))))
  }
  def getQualifierSummary(qualifier: Qualifier): QualifierSummary = {
    QualifierSummary(qualifier.format.baseFormatString, qualifier.variableDefinitions)
  }
  def getPropertyDefinitionSummary(pd: PropertyDefinitionOnType): PropertyDefinitionSummary = {
    PropertyDefinitionSummary(pd.symbol, pd.qualifiedSymbol, pd.name, pd.parentTypeConditions.requiredParentQualifier.map(_.symbol), pd.parentTypeConditions.requiredParentObjects.objectDefinitions.map(_.symbol))
  }
  def getAllFromContext(availableEntries: AvailableEntries): Map[String, TypeDefinitionSummary] = {
    availableEntries.typeDefinitions.view.mapValues(d => getSummary(d)(availableEntries)).toMap
  }
}

object StandalonePropertyDefinitionSummary {
  def getAllFromContext(availableEntries: AvailableEntries): Map[String, StandalonePropertyDefinitionSummary] = {
    availableEntries.standalonePropertyDefinitions
      .map(d => d.symbol -> StandalonePropertyDefinitionSummary(
        d.symbol,
        d.qualifiedSymbol,
        d.name,
        d.mainVariableDefinition))
      .toMap
  }
}

object TypeRelationDefinitionSummary {
  def getAllFromContext(availableEntries: AvailableEntries): Map[String, TypeRelationDefinitionSummary] = {
    availableEntries.typeRelationDefinitions.map(d => d.symbol -> TypeRelationDefinitionSummary(d.symbol, d.linkingPhrase)).toMap
  }
}
