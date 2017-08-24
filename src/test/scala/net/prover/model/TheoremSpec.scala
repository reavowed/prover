package net.prover.model

import java.nio.file.Paths

import net.prover.model.Inference.RearrangementType
import net.prover.model.components._
import net.prover.model.entries.Axiom
import net.prover.model.proof.ProofOutline.{AssertionStep, AssumptionStep}
import net.prover.model.proof._

class TheoremSpec extends ProverSpec {

  def axiom[T : PremiseConverter](
    name: String,
    premiseSources: Seq[T],
    conclusion: Statement,
    rearrangementType: RearrangementType = RearrangementType.NotRearrangement
  ): Axiom = {
    Axiom(name, name.formatAsKey, "test-chapter", "Test Chapter", "test-book", "Test Book", premiseSources, conclusion, rearrangementType)
  }

  "theorem parser" should {
    implicit def assertionStepFromStatement(statement: Statement): ProofOutline.Step = {
      AssertionStep(statement, location = ProofOutline.Location("test.book", 1))
    }

    implicit def assumptionStepFromStatementPair(tuple: (Statement, Statement)): ProofOutline.Step = {
      AssumptionStep(tuple._1, Seq(assertionStepFromStatement(tuple._2)))
    }

    implicit def assumptionStepFromStatementAndStatements(tuple: (Statement, Seq[Statement])): ProofOutline.Step = {
      AssumptionStep(tuple._1, tuple._2.map(assertionStepFromStatement))
    }

    def prove[T : PremiseConverter](
      premiseSources: Seq[T],
      proofSteps: Seq[ProofOutline.Step],
      inferences: Seq[Inference]
    ): Proof = {
      Proof.fillInOutline(
        premiseSources,
        ProofOutline(proofSteps),
        inferences,
        Nil)
    }

    def checkProof[T : PremiseConverter](
      premiseSources: Seq[T],
      proofSteps: Seq[ProofOutline.Step],
      inferences: Seq[Inference]
    ) = {
      val proof = prove(premiseSources, proofSteps, inferences)
      proof.conclusion mustEqual proofSteps.ofType[ProofOutline.StepWithAssertion].last.innermostAssertionStep.assertion
      proof.matchesOutline(ProofOutline(proofSteps)) must beTrue
      val serializedProof = proof.serialized
      val deserializedProof = Proof.parser.parse(Tokenizer.fromString(serializedProof, Paths.get("")))._1
      CachedProof(Paths.get(""), premiseSources, deserializedProof).validate(inferences) must beSome(proof)
    }

    "not prove an unfounded statement" in {
      prove(Nil, Seq(φ), Nil) must throwAn[Exception]
    }

    val repetition = axiom("Repetition", Seq(φ), φ)
    val deduction = axiom("Deduction", Seq(Fact.Deduced(φ, ψ)), Implication(φ, ψ))
    val modusPonens = axiom("Modus Ponens", Seq(Implication(φ, ψ), φ), ψ)
    val implicationIsReflexive = axiom("Implication Is Reflexive", Nil, Implication(φ, φ))
    val extractLeftConjunct = axiom("Extract Left Conjunct", Seq(Conjunction(φ, ψ)), φ, RearrangementType.Simplification)
    val combineConjunction = axiom("Combine Conjunction", Seq(φ, ψ), Conjunction(φ, ψ), RearrangementType.Expansion)
    val addRightDisjunct = axiom("Add Right Disjunct", Seq(ψ), Disjunction(φ, ψ), RearrangementType.Expansion)

    "prove the conclusion of a premiseless inference" in {
      checkProof(
        Nil,
        Seq(Implication(ψ, ψ)),
        Seq(implicationIsReflexive))
    }

    "prove the conclusion of an inference whose premises match proven statements" in {
      checkProof(
        Seq(Implication(Implication(Implication(ψ, χ), ψ), ψ), Implication(Implication(ψ, χ), ψ)),
        Seq(ψ),
        Seq(modusPonens))
    }

    "not prove the conclusion of an inference whose premises can't be matched" in {
      prove(
        Seq(Implication(φ, χ), ψ),
        Seq(φ),
        Seq(modusPonens)
      ) must throwAn[Exception]
    }

    "prove the conclusion of an inference with a deduced premise that matches an assumption" in {
      checkProof(
        Seq(χ),
        Seq(ψ -> χ, Implication(ψ, χ)),
        Seq(repetition, deduction))
    }

    "prove an inference conclusion by simplifying a premise" in {
      val anythingImpliesATrueStatement = axiom(
        "Anything Implies a True Statement",
        Seq(φ),
        Implication(ψ, φ))

      checkProof(
        Seq(Conjunction(φ, ψ)),
        Seq(Implication(χ, φ)),
        Seq(extractLeftConjunct, anythingImpliesATrueStatement))
    }

    "prove a statement by rearranging" in {
      checkProof(
        Seq(Conjunction(φ, ψ), χ),
        Seq(Disjunction(ψ, Conjunction(χ, φ))),
        Seq(extractLeftConjunct, combineConjunction, addRightDisjunct))
    }
  }
}
