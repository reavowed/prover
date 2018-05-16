package net.prover.model

import net.prover.model.entries.{Axiom, ChapterEntry}
import net.prover.model.expressions.Statement
import net.prover.model._

class AxiomSpec extends ProverSpec {
  val chapterKey = Chapter.Key("", Book.Key(""))

  def parseAxiom(text: String*): Axiom = {
    Axiom.parser(s => ChapterEntry.Key(s.formatAsKey, chapterKey)).parseAndDiscard(text.mkString("\n"))
  }

  def axiom(title: String, key: String, premises: Seq[PremiseMagnet], conclusion: Statement): Axiom = {
    Axiom(
      title,
      ChapterEntry.Key(key, chapterKey),
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
