package net.prover.model

import net.prover.model.expressions.Statement
import net.prover.model.entries.{Axiom, AxiomOutline}
import net.prover.model.proof.Fact

class AxiomSpec extends ProverSpec {

  def parseAxiom(text: String*): Axiom = {
    val outline = AxiomOutline.parser("", "").parseAndDiscard(text.mkString("\n"))
    outline.expand(outline.name.formatAsKey, "Test Chapter", "Test Book")
  }

  def axiom(title: String, key: String, premises: Seq[PremiseMagnet], conclusion: Statement): Axiom = {
    Axiom(
      title,
      key,
      "test-chapter",
      "Test Chapter",
      "test-book",
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

    "parse an axiom with a deduced premise" in {
      parseAxiom(
        "Deduction",
        "premise proves φ ψ",
        "conclusion → φ ψ"
      ) mustEqual axiom(
        "Deduction",
        "deduction",
        Seq(Fact.Deduced(φ, ψ)),
        Implication(φ, ψ))
    }
  }
}
