package net.prover.model

import net.prover.model.entries.{Axiom, AxiomOutline}

class AxiomSpec extends ProverSpec {

  def parseAxiom(text: String*): Axiom = {
    val outline = AxiomOutline.parser("", "").parseAndDiscard(text.mkString("\n"))
    outline.expand(outline.name.formatAsKey, "Test Chapter", "Test Book")
  }

  "axiom parser" should {
    "parse an axiom with no premises" in {
      parseAxiom(
        "Axiom of Extensionality",
        "conclusion ∀ x ∀ y ∀ z → ↔ ∈ z x ∈ z y ↔ ∈ x z ∈ y z"
      ) mustEqual Axiom(
        "Axiom of Extensionality",
        "axiom-of-extensionality",
        "test-chapter",
        "Test Chapter",
        "test-book",
        "Test Book",
        Nil,
        ForAll(x, ForAll(y, ForAll(z, Implication(
          Equivalence(ElementOf(z, x), ElementOf(z, y)),
          Equivalence(ElementOf(x, z), ElementOf(y, z)))))))
    }

    "parse an axiom with a single premise" in {
      parseAxiom(
        "Restate",
        "premise φ",
        "conclusion φ"
      ) mustEqual Axiom(
        "Restate",
        "restate",
        "test-chapter",
        "Test Chapter",
        "test-book",
        "Test Book",
        Seq(Premise.DirectPremise(φ)),
        φ)
    }

    "parse an axiom with two premises" in {
      parseAxiom(
        "Eliminate Implication",
        "premise → φ ψ",
        "premise φ",
        "conclusion ψ"
      ) mustEqual Axiom(
        "Eliminate Implication",
        "eliminate-implication",
        "test-chapter",
        "Test Chapter",
        "test-book",
        "Test Book",
        Seq(Premise.DirectPremise(Implication(φ, ψ)), Premise.DirectPremise(φ)),
        ψ)
    }

    "parse an axiom with a deduced premise" in {
      parseAxiom(
        "Deduction",
        "premise proves φ ψ",
        "conclusion → φ ψ"
      ) mustEqual Axiom(
        "Deduction",
        "deduction",
        "test-chapter",
        "Test Chapter",
        "test-book",
        "Test Book",
        Seq(Premise.DeducedPremise(φ, ψ)),
        Implication(φ, ψ))
    }
  }
}
