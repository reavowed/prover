package net.prover.model

import net.prover.model.Inference.DeducedPremise

class TheoremSpec extends ProverSpec {
  "theorem parser" should {
    def parseTheorem(text: String*)(implicit context: Context): Theorem = {
      Theorem.parser(context).parseAndDiscard(text.mkString("\n"))
    }
    def contextWith(inferences: Inference*): Context = {
      implicitly[Context].copy(inferences = inferences)
    }

    "not accept an unfounded assertion" in {
      parseTheorem(
        "Anything Is True",
        "prove φ",
        "qed"
      ) must throwAn[Exception]
    }

    "accept assertion of the conclusion of a known premiseless inference" in {
      val previousInference = new Axiom(
        "Implication Is Reflexive",
        Nil,
        Implication(φ, φ))

      val theorem = parseTheorem(
        "X",
        "prove → ψ ψ",
        "qed")(
        contextWith(previousInference))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(Implication(ψ, ψ))
    }

    "accept assertion of the conclusion of an inference whose premises match proven statements" in {
      val previousInference = new Axiom(
        "Modus Ponens",
        Seq(Implication(φ, ψ), φ),
        ψ)

      val theorem = parseTheorem(
        "X",
        "premise → → → ψ χ ψ ψ",
        "premise → → ψ χ ψ",
        "prove ψ",
        "qed")(
        contextWith(previousInference))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(ψ)
    }

    "not accept assertion of the conclusion of an inference whose premises can't be matched" in {
      val previousInference = new Axiom(
        "X",
        Seq(φ),
        Implication(φ, φ))

      parseTheorem(
        "X",
        "premise ψ",
        "prove → χ χ",
        "qed")(
        contextWith(previousInference)
      ) must throwAn[Exception]
    }

    "accept assertion of the conclusion of an inference with a deduced premise that matches an assumption" in {
      val repeatAxiom = new Axiom(
        "Repeat",
        Seq(φ),
        φ)
      val deductionAxiom = new Axiom(
        "Deduction",
        Seq(DeducedPremise(φ, ψ)),
        Implication(φ, ψ))

      val theorem = parseTheorem(
        "X",
        "premise χ",
        "assume ψ {",
        "  prove χ",
        "}",
        "prove → ψ χ",
        "qed")(
        contextWith(repeatAxiom, deductionAxiom))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(Implication(ψ, χ))
    }

    "accept assertion of the conclusion of an inference with a deduced premise that matches another inference" in {
      val repeatAxiom = new Axiom(
        "Repeat",
        Seq(φ),
        φ)
      val contradictionAxiom = new Axiom(
        "Contradiction",
        Seq(DeducedPremise(φ, ψ), DeducedPremise(φ, Negation(ψ))),
        Negation(φ))

      val theorem = parseTheorem(
        "X",
        "premise proves φ ¬ φ",
        "prove ¬ φ",
        "qed")(
        contextWith(repeatAxiom, contradictionAxiom))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(Negation(φ))
    }
  }
}
