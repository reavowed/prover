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
        ForAll(1, ForAll(2, ForAll(3, Implication(
          Equivalence(ElementOf(3, 1), ElementOf(3, 2)),
          Equivalence(ElementOf(1, 3), ElementOf(2, 3)))))))
    }
  }

  "axiom" should {
    "apply directly to theorem" in {
      val axiom = Axiom(
        "ax-extensionality",
        "Axiom of Extensionality",
        ForAll(1, ForAll(2, ForAll(3, Implication(
          Equivalence(ElementOf(3, 1), ElementOf(3, 2)),
          Equivalence(ElementOf(1, 3), ElementOf(2, 3)))))))

      axiom.readAndUpdateTheoremBuilder(TheoremBuilder(), "2 1 3", defaultContext).steps.head.statement mustEqual
        ForAll(2, ForAll(1, ForAll(3, Implication(
          Equivalence(ElementOf(3, 2), ElementOf(3, 1)),
          Equivalence(ElementOf(2, 3), ElementOf(1, 3))))))
    }
  }

}
