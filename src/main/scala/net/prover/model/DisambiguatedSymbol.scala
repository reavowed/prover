package net.prover.model

case class DisambiguatedSymbol(baseSymbol: String, disambiguator: Option[String]) {
  val serialized: String = disambiguator.map(baseSymbol +  _).getOrElse(baseSymbol)
  val forDisplay: String = disambiguator.map(baseSymbol + "_" + _).getOrElse(baseSymbol)
}
