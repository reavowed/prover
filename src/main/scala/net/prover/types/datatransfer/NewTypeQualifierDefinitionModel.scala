package net.prover.types.datatransfer

case class NewTypeQualifierDefinitionModel(
  symbol: String,
  parentType: String,
  qualifierTermNames: String,
  qualifierFormat: String,
  name: String,
  definition: String)
