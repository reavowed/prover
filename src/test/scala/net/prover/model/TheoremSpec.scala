package net.prover.model

import java.nio.file.Paths

import net.prover.model.Inference.RearrangementType
import net.prover.model.expressions._
import net.prover.model.entries.{Axiom, StatementDefinition}
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
    implicit def assertionStepFromStatement(assertion: Statement): StepOutline = {
      StepOutline.Assertion(assertion, Some(FileLocation("test.book", 1)))
    }

    implicit def assumptionStepFromStatementPair(tuple: (Statement, Statement)): StepOutline = {
      StepOutline.Assumption(tuple._1, Seq(assertionStepFromStatement(tuple._2)))
    }

    implicit def assumptionStepFromStatementAndStatements(tuple: (Statement, Seq[Statement])): StepOutline = {
      StepOutline.Assumption(tuple._1, tuple._2.map(assertionStepFromStatement))
    }

    def prove[T : PremiseConverter](
      premiseSources: Seq[T],
      proofSteps: Seq[StepOutline],
      inferences: Seq[Inference],
      transformations: Seq[StatementDefinition] = Nil
    ): Proof = {
      Proof.fillInOutline(
        premiseSources,
        ProofOutline(proofSteps),
        inferences,
        Nil,
        transformations)
    }

    def checkProof[T : PremiseConverter](
      premiseSources: Seq[T],
      proofSteps: Seq[StepOutline],
      inferences: Seq[Inference],
      transformations: Seq[StatementDefinition] = Nil
    ) = {
      val proof = prove(premiseSources, proofSteps, inferences, transformations)
      proof.conclusion mustEqual proofSteps.ofType[StepOutline.WithAssertion].last.innermostAssertionStep.assertion
      val cachedProof = CachedProof(Paths.get(""), premiseSources, proof.steps.map(_.cached))
      cachedProof.steps.matchOutlines(proofSteps) must beTrue
      val serializedProof = cachedProof.serialized
      val deserializedProof = CachedProof.parser(Paths.get("")).parse(Tokenizer.fromString(serializedProof, Paths.get("")))._1
      deserializedProof mustEqual cachedProof
      deserializedProof.validate(inferences, transformations) must beSome(proof)
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

    val generalization = axiom(
      "Generalization",
      Seq(Fact.ScopedVariable(φ.!(FunctionParameter("x", 0)))("x")),
      ForAll("x")(φ.!(FunctionParameter("x", 0))))
    val specification = axiom(
      "Specification",
      Seq(ForAll("x")(φ.!(FunctionParameter("x", 0)))),
      φ(a))
    val substitutionOfEquals = axiom(
      "Substitution of Equals",
      Seq(Equals(a, b), φ(a)),
      φ(b))

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

    "prove an inference conclusion with a predicate" in {
      checkProof(
        Seq(Equals(a, b), Equals(a, a)),
        Seq(Equals(b, a)),
        Seq(substitutionOfEquals))
    }

    "prove an inference substituting a named predicate for another named predicate" in {
      checkProof(
        Seq(Equals(b, a), φ(b)),
        Seq(φ(a)),
        Seq(substitutionOfEquals))
    }

    "prove an inference with a scoped variable" in {
      val equalityIsReflexive = axiom(
        "Equality Is Reflexive",
        Nil,
        Equals(a, a))
      checkProof(
        Nil,
        Seq(
          StepOutline.ScopedVariable("y", Seq(Equals.!(FunctionParameter("y", 0), FunctionParameter("y", 0)))),
          ForAll("y")(Equals.!(FunctionParameter("y", 0), FunctionParameter("y", 0)))),
        Seq(equalityIsReflexive, generalization))
    }

    "prove a conclusion containing a bound variable" in {
      val equivalenceOfSubstitutedEquals = axiom(
        "Equivalence of Substituted Equals",
        Seq(Equals(a, b)),
        Equivalence(φ(a), φ(b)))
      checkProof(
        Seq(Equals(b, c)),
        Seq(
          StepOutline.ScopedVariable("x", Seq(Equivalence.!(
            ElementOf.!(FunctionParameter("x", 0), b.^),
            ElementOf.!(FunctionParameter("x", 0), c.^)))),
          ForAll("x")(Equivalence.!(
            ElementOf.!(FunctionParameter("x", 0), b.^),
            ElementOf.!(FunctionParameter("x", 0), c.^)))),
        Seq(equivalenceOfSubstitutedEquals, generalization))
    }

    "prove a transformed inference" in {
      checkProof(
        Seq(
          ForAll("x")(φ.!(FunctionParameter("x", 0))),
          ForAll("x")(Implication.!(φ.!(FunctionParameter("x", 0)), ψ.!(FunctionParameter("x", 0))))),
        Seq(
          ForAll("x")(ψ.!(FunctionParameter("x", 0)))),
        Seq(generalization, specification, modusPonens),
        Seq(ForAll))
    }

    "prove a partially transformed inference" in {
      checkProof(
        Seq(
          ForAll("x")(Implication.!(φ.!(FunctionParameter("x", 0)), ψ.!(FunctionParameter("x", 0)))),
          φ(a)),
        Seq(ψ(a)),
        Seq(generalization, specification, modusPonens),
        Seq(ForAll))
    }
  }
}
