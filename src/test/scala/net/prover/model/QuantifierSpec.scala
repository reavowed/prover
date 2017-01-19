package net.prover.model

class QuantifierSpec extends ProverSpec {
  "quantifier parser" should {
    "parse a quantifier" in {
      Quantifier.parse("∀ () ()", defaultContext) mustEqual Quantifier("∀", None, DistinctVariables.empty)
    }

    "parse a quantifier with a definition" in {
      Quantifier.parse("∃ (¬ ∀ 1 ¬ 1) ()", defaultContext) mustEqual
        Quantifier("∃", Some(Negation(ForAll("z", Negation(1)))), DistinctVariables.empty)
    }

    "parse a quantifier with distinct variable requirements" in {
      Quantifier.parse("∃! (∀ 2 ∃ 1 ↔ 1 = 1 2) (2 1)", defaultContext) mustEqual
        Quantifier(
          "∃!",
          Some(ForAll("y", Exists("z", Equivalence(1, Equals("z", "y"))))),
          DistinctVariables(Map(TermVariable("y") -> Variables(Seq(StatementVariable(1)), Nil))))
    }
  }
}
