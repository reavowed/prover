package net.prover.model

class QuantifierSpec extends ProverSpec {
  "quantifier parser" should {
    "parse a quantifier" in {
      Quantifier.parse("∀", defaultContext) mustEqual Quantifier("∀", None)
    }

    "parse a quantifier with a definition" in {
      Quantifier.parse("∃ ¬ ∀ 1 ¬ 1", defaultContext) mustEqual
        Quantifier("∃", Some(Negation(ForAll(1, Negation(1)))))
    }
  }
}
