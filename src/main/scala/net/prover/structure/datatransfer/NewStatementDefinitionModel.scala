package net.prover.structure.datatransfer

case class NewStatementDefinitionModel(
  symbol: String,
  components: String,
  name: String,
  format: String,
  definition: Option[String],
  shorthand: String,
  attributes: String)
