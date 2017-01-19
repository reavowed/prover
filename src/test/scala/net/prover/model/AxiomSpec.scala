package net.prover.model

class AxiomSpec extends ProverSpec {
  "axiom parser" should {
    "parse an axiom" in {
      Axiom.parse(
        "ax-extensionality \"Axiom of Extensionality\" ∀ 1 ∀ 2 ∀ 3 → ↔ ∈ 3 1 ∈ 3 2 ↔ ∈ 1 3 ∈ 2 3",
        defaultContext
      ) mustEqual Axiom(
        "ax-extensionality",
        "Axiom of Extensionality",
        ForAll("z", ForAll("y", ForAll("x", Implication(
          Equivalence(ElementOf("x", "z"), ElementOf("x", "y")),
          Equivalence(ElementOf("z", "x"), ElementOf("y", "x")))))))
    }
  }

  "axiom" should {
    "apply directly to theorem" in {
      val axiom = Axiom(
        "ax-extensionality",
        "Axiom of Extensionality",
        ForAll("z", ForAll("y", ForAll("x", Implication(
          Equivalence(ElementOf("x", "z"), ElementOf("x", "y")),
          Equivalence(ElementOf("z", "x"), ElementOf("y", "x")))))))

      axiom.readAndUpdateTheoremBuilder(TheoremBuilder(), "2 1 3", defaultContext).steps.head.statement mustEqual
        ForAll("y", ForAll("z", ForAll("x", Implication(
          Equivalence(ElementOf("x", "y"), ElementOf("x", "z")),
          Equivalence(ElementOf("y", "x"), ElementOf("z", "x"))))))
    }
  }

}
