package net.prover.model

class QuantifierSpec extends ProverSpec {
  "quantifier parser" should {
    "parse a quantifier" in {
      Quantifier.parse("∀ () ()", defaultContext) mustEqual Quantifier("∀", None, DistinctVariableRequirements.empty)
    }

    "parse a quantifier with a definition" in {
      Quantifier.parse("∃ (¬ ∀ 1 ¬ 1) ()", defaultContext) mustEqual
        Quantifier("∃", Some(Negation(ForAll(1, Negation(1)))), DistinctVariableRequirements.empty)
    }

    "parse a quantifier with distinct variable requirements" in {
      Quantifier.parse("∃! (∀ 2 ∃ 1 ↔ 1 = 1 2) (2 1)", defaultContext) mustEqual
        Quantifier(
          "∃!",
          Some(ForAll(2, Exists(1, Equivalence(1, Equals(1, 2))))),
          DistinctVariableRequirements(Map(TermVariable(2) -> Variables(Seq(StatementVariable(1)), Nil))))
    }
  }
}
