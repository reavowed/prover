package net.prover.model

import net.prover.model.entries.Axiom
import net.prover.model.expressions.Statement

class AxiomSpec extends ProverSpec {

  def parseAxiom(text: String*): Axiom = {
    Axiom.parser("Test Chapter", "Test Book", identity).parseAndDiscard(text.mkString("\n"))
  }

  def axiom(title: String, key: String, premises: Seq[PremiseMagnet], conclusion: Statement): Axiom = {
    Axiom(
      title,
      key,
      "Test Chapter",
      "Test Book",
      premises,
      conclusion)
  }

  "axiom parser" should {
    "parse an axiom with no premises" in {
      parseAxiom(
        "Equality Is Reflexive",
        "conclusion = a a"
      ) mustEqual axiom(
        "Equality Is Reflexive",
        "equality-is-reflexive",
        Nil,
        Equals(a, a))
    }

    "parse an axiom with a single premise" in {
      parseAxiom(
        "Restate",
        "premise φ",
        "conclusion φ"
      ) mustEqual axiom(
        "Restate",
        "restate",
        Seq(φ),
        φ)
    }

    "parse an axiom with two premises" in {
      parseAxiom(
        "Eliminate Implication",
        "premise → φ ψ",
        "premise φ",
        "conclusion ψ"
      ) mustEqual axiom(
        "Eliminate Implication",
        "eliminate-implication",
        Seq(Implication(φ, ψ), φ),
        ψ)
    }
  }
}
