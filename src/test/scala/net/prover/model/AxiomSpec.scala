package net.prover.model

class AxiomSpec extends ProverSpec {

  def parseAxiom(text: String): Axiom = {
    Axiom.parse(text, defaultContext)
  }

  "axiom parser" should {
    "parse a simple axiom" in {
      parseAxiom(
        "ax-extensionality (Axiom of Extensionality) () () (∀ 1 ∀ 2 ∀ 3 → ↔ ∈ 3 1 ∈ 3 2 ↔ ∈ 1 3 ∈ 2 3) () ()"
      ) mustEqual Axiom(
        "ax-extensionality",
        "Axiom of Extensionality",
        None,
        Nil,
        ForAll("z", ForAll("y", ForAll("x", Implication(
          Equivalence(ElementOf("x", "z"), ElementOf("x", "y")),
          Equivalence(ElementOf("z", "x"), ElementOf("y", "x")))))),
        Nil,
        DistinctVariables.empty)
    }

    "parse an axiom with a single premise" in {
      parseAxiom("restate (Restate) () (1) (1) () ()") mustEqual Axiom(
        "restate",
        "Restate",
        None,
        Seq(StatementVariable(1)),
        StatementVariable(1),
        Nil,
        DistinctVariables.empty)
    }

    "parse an axiom with two premises" in {
      parseAxiom("eliminateImplication (Eliminate Implication) () (→ 1 2, 1) (2) () ()") mustEqual
        Axiom(
          "eliminateImplication",
          "Eliminate Implication",
          None,
          Seq(Implication(StatementVariable(1), StatementVariable(2)), StatementVariable(1)),
          StatementVariable(2),
          Nil,
          DistinctVariables.empty)
    }

    "parse an axiom with a discharged assumption" in {
      parseAxiom(
        "introduceImplication (Introduce Implication) (1) (2) (→ 1 2) () ()"
      ) mustEqual Axiom(
        "introduceImplication",
        "Introduce Implication",
        Some(StatementVariable(1)),
        Seq(StatementVariable(2)),
        Implication(StatementVariable(1), StatementVariable(2)),
        Nil,
        DistinctVariables.empty)
    }

    "not allow multiple discharged assumptions" in {
      parseAxiom(
        "introduceImplication (Introduce Implication) (1, 2) (3) (→ → 1 2 3) () ()"
      ) must throwAn[Exception]
    }

    "parse a rule with arbitrary and distinct variables" in {
      parseAxiom("" +
        "introduceForall (Introduce Forall) () (sub 2 1 1) (∀ 1 1) (2) (2 1)"
      ) mustEqual Axiom(
        "introduceForall",
        "Introduce Forall",
        None,
        Seq(StatementVariableWithReplacement(1, "y", "z")),
        ForAll("z", 1),
        Seq("y"),
        DistinctVariables(Map(TermVariable("y") -> Variables(Seq(StatementVariable(1)), Nil))))
    }
  }

  "axiom" should {
    "apply directly to theorem" in {
      val axiom = Axiom(
        "ax-extensionality",
        "Axiom of Extensionality",
        None,
        Nil,
        ForAll("z", ForAll("y", ForAll("x", Implication(
          Equivalence(ElementOf("x", "z"), ElementOf("x", "y")),
          Equivalence(ElementOf("z", "x"), ElementOf("y", "x")))))),
        Nil,
        DistinctVariables.empty)

      axiom.readAndUpdateTheoremBuilder(TheoremBuilder(), "2 1 3", defaultContext).steps.head.statement mustEqual
        ForAll("y", ForAll("z", ForAll("x", Implication(
          Equivalence(ElementOf("x", "y"), ElementOf("x", "z")),
          Equivalence(ElementOf("y", "x"), ElementOf("z", "x"))))))
    }
  }

}
