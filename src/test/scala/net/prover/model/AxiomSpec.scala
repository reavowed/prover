package net.prover.model

class AxiomSpec extends ProverSpec {

  def parseAxiom(text: String): Axiom = {
    Axiom.parser(defaultContext).parseAndDiscard(text)
  }

  "axiom parser" should {
    "parse a simple axiom" in {
      parseAxiom(
        "ax-extensionality (Axiom of Extensionality) () () (∀ x ∀ y ∀ z → ↔ ∈ z x ∈ z y ↔ ∈ x z ∈ y z) () ()"
      ) mustEqual Axiom(
        "ax-extensionality",
        "Axiom of Extensionality",
        None,
        Nil,
        ForAll(x, ForAll(y, ForAll(z, Implication(
          Equivalence(ElementOf(z, x), ElementOf(z, y)),
          Equivalence(ElementOf(x, z), ElementOf(y, z)))))),
        Nil,
        DistinctVariables.empty)
    }

    "parse an axiom with a single premise" in {
      parseAxiom("restate (Restate) () (φ) (φ) () ()") mustEqual Axiom(
        "restate",
        "Restate",
        None,
        Seq(φ),
        φ,
        Nil,
        DistinctVariables.empty)
    }

    "parse an axiom with two premises" in {
      parseAxiom("eliminateImplication (Eliminate Implication) () (→ φ ψ, φ) (ψ) () ()") mustEqual
        Axiom(
          "eliminateImplication",
          "Eliminate Implication",
          None,
          Seq(Implication(φ, ψ), φ),
          ψ,
          Nil,
          DistinctVariables.empty)
    }

    "parse an axiom with a discharged assumption" in {
      parseAxiom(
        "introduceImplication (Introduce Implication) (φ) (ψ) (→ φ ψ) () ()"
      ) mustEqual Axiom(
        "introduceImplication",
        "Introduce Implication",
        Some(φ),
        Seq(ψ),
        Implication(φ, ψ),
        Nil,
        DistinctVariables.empty)
    }

    "not allow multiple discharged assumptions" in {
      parseAxiom(
        "introduceImplication (Introduce Implication) (φ, ψ) (3) (→ → 1 2 3) () ()"
      ) must throwAn[Exception]
    }

    "parse a rule with arbitrary and distinct variables" in {
      parseAxiom("" +
        "introduceForall (Introduce Forall) () (sub y x φ) (∀ x φ) (y) (y φ)"
      ) mustEqual Axiom(
        "introduceForall",
        "Introduce Forall",
        None,
        Seq(StatementVariableWithReplacement(φ, y, x)),
        ForAll(x, φ),
        Seq(y),
        DistinctVariables(Map(y -> Variables(Seq(φ), Nil))))
    }
  }

  "axiom" should {
    "apply directly to theorem" in {
      val axiom = Axiom(
        "ax-extensionality",
        "Axiom of Extensionality",
        None,
        Nil,
        ForAll(x, ForAll(y, ForAll(z, Implication(
          Equivalence(ElementOf(z, x), ElementOf(z, y)),
          Equivalence(ElementOf(x, z), ElementOf(y, z)))))),
        Nil,
        DistinctVariables.empty)

      axiom.readAndUpdateTheoremBuilder(TheoremBuilder(), "y x z", defaultContext).steps.head.statement mustEqual
        ForAll(y, ForAll(x, ForAll(z, Implication(
          Equivalence(ElementOf(z, y), ElementOf(z, x)),
          Equivalence(ElementOf(y, z), ElementOf(x, z))))))
    }
  }

}
