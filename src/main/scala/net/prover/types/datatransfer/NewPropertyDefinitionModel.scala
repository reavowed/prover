package net.prover.types.datatransfer

case class NewPropertyDefinitionModel(
  symbol: String,
  parentType: String,
  requiredParentQualifier: String,
  requiredParentObjects: String,
  name: String,
  definingStatement: String,
  ownTermNames: String,
  parentTerms: String)
