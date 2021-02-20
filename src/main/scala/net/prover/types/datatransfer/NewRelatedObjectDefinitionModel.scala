package net.prover.types.datatransfer

case class NewRelatedObjectDefinitionModel(
  symbol: String,
  mainVariableDefinition: String,
  parentType: String,
  requiredParentQualifier: String,
  requiredParentObjects: String,
  name: String,
  definingStatement: String)
