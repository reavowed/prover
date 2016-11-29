package net.prover.model

class QuantifierSpec extends ProverSpec {
  "quantifier parser" should {
    "parse a quantifier" in {
      Quantifier.parse("∀", defaultContext) mustEqual Quantifier("∀")
    }
  }
}
