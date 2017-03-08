package net.prover.model

import net.prover.model.Inference.{DeducedPremise, DirectPremise}

class AxiomSpec extends ProverSpec {

  def parseAxiom(text: String*): Axiom = {
    Axiom.parser(defaultContext).parseAndDiscard(text.mkString("\n"))
  }

  "axiom parser" should {
    "parse an axiom with no premises" in {
      parseAxiom(
        "Axiom of Extensionality",
        "conclusion ∀ x ∀ y ∀ z → ↔ ∈ z x ∈ z y ↔ ∈ x z ∈ y z"
      ) mustEqual Axiom(
        "Axiom of Extensionality",
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
        Seq(DirectPremise(φ)),
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
        Seq(DirectPremise(Implication(φ, ψ)), DirectPremise(φ)),
        ψ)
    }

    "parse an axiom with a deduced premise" in {
      parseAxiom(
        "Deduction",
        "premise proves φ ψ",
        "conclusion → φ ψ"
      ) mustEqual Axiom(
        "Deduction",
        Seq(DeducedPremise(φ, ψ)),
        Implication(φ, ψ))
    }

//    "parse an axiom with arbitrary and distinct variables" in {
//      parseAxiom(
//        "Introduce Forall",
//        "premise sub y x φ",
//        "conclusion ∀ x φ",
//        "arbitrary-variables (y)",
//        "distinct-variables (y φ)"
//      ) mustEqual Axiom(
//        "Introduce Forall",
//        Seq(DirectPremise(StatementVariableWithReplacement(φ, y, x))),
//        ProvenStatement(
//          ForAll(x, φ),
//          Seq(y),
//          DistinctVariables(Map(y -> Variables(Seq(φ), Nil)))))
//    }
  }
}
