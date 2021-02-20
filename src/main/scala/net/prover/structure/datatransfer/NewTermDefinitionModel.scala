package net.prover.structure.datatransfer

case class NewTermDefinitionModel(
  symbol: String,
  components: String,
  disambiguator: String,
  name: String,
  format: String,
  premises: Seq[String],
  definition: String,
  shorthand: String,
  attributes: String)
