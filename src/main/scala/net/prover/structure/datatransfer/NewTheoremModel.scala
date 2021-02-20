package net.prover.structure.datatransfer

case class NewTheoremModel(
  name: String,
  variableDefinitions: String,
  premises: Seq[String],
  conclusion: String)
