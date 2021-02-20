package net.prover.types.datatransfer

case class NewTypeDefinitionModel(
  symbol: String,
  mainVariableDefinition: String,
  qualifierVariableDefinitions: String,
  qualifierFormat: String,
  name: String,
  definition: String)
