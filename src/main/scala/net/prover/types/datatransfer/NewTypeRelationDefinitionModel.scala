package net.prover.types.datatransfer

case class NewTypeRelationDefinitionModel(
  symbol: String,
  firstType: String,
  firstVariableDefinition: String,
  secondType: String,
  secondVariableDefinition: String,
  linkingPhrase: String,
  name: String,
  definingStatement: String)
