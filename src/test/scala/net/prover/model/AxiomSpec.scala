package net.prover.model

import net.prover.model.entries.Axiom

class AxiomSpec extends ProverSpec {

  def parseAxiom(text: String*): Axiom = {
    implicit val ec = entryContext
    Axiom.parser.parseAndDiscard(text.mkString("\n"))
  }

  "axiom parser" should {
    "parse an axiom with no premises" in {
      parseAxiom(
        "Equality Is Reflexive",
        "conclusion = a a"
      ) mustEqual Axiom(
        "Equality Is Reflexive",
        Nil,
        Equals(a, a))
    }

    "parse an axiom with a single premise" in {
      parseAxiom(
        "Restate",
        "premise φ",
        "conclusion φ"
      ) mustEqual Axiom(
        "Restate",
        Seq(φ),
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
        Seq(Implication(φ, ψ), φ),
        ψ)
    }
  }
}
