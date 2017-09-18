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
    implicit def assertionStepFromStatement(statement: Statement): StepOutline = {
      StepOutline.Assertion(statement, Some(FileLocation("test.book", 1)))
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
      val substitutionOfEquals = axiom(
        "Substitution of Equals",
        Seq(Equals(x, y), PredicateApplication("φ", x)),
        PredicateApplication("φ", y))
      checkProof(
        Seq(Equals(x, y), Equals(x, x)),
        Seq(Equals(y, x)),
        Seq(substitutionOfEquals))
    }

    "prove an inference substituting a named predicate for another named predicate" in {
      val substitutionOfEquals = axiom(
        "Substitution of Equals",
        Seq(Equals(x, y), PredicateApplication("φ", x)),
        PredicateApplication("φ", y))
      checkProof(
        Seq(Equals(y, x), PredicateApplication("φ", y)),
        Seq(PredicateApplication("φ", x)),
        Seq(substitutionOfEquals))
    }

    "prove an inference with a scoped variable" in {
      val equalityIsReflexive = axiom(
        "Equality Is Reflexive",
        Nil,
        Equals(x, x))
      val generalization = axiom(
        "Generalization",
        Seq(Fact.Bound(PredicateApplication("φ", BoundVariable(0)("x")))("x")),
        ForAll("x")(PredicateApplication("φ", BoundVariable(0)("x"))))
      checkProof(
        Nil,
        Seq(
          StepOutline.ScopedVariable("y", Seq(Equals(BoundVariable(0)("y"), BoundVariable(0)("y")))),
          ForAll("y")(Equals(BoundVariable(0)("y"), BoundVariable(0)("y")))),
        Seq(equalityIsReflexive, generalization))
    }

    "prove a conclusion containing a bound variable" in {
      val equalityIsReflexive = axiom(
        "Equivalence of Substituted Equals",
        Seq(Equals(a, b)),
        Equivalence(PredicateApplication("φ", x), PredicateApplication("φ", y)))
      val generalization = axiom(
        "Generalization",
        Seq(Fact.Bound(PredicateApplication("φ", BoundVariable(0)("x")))("x")),
        ForAll("x")(PredicateApplication("φ", BoundVariable(0)("x"))))
      checkProof(
        Seq(Equals(x, y)),
        Seq(
          StepOutline.ScopedVariable("z", Seq(Equivalence(ElementOf(BoundVariable(0)("z"), a), ElementOf(BoundVariable(0)("z"), b)))),
          ForAll("z")(Equivalence(ElementOf(BoundVariable(0)("z"), a), ElementOf(BoundVariable(0)("z"), b)))),
        Seq(equalityIsReflexive, generalization))
    }

    "prove a transformed inference" in {
      val generalization = axiom(
        "Generalization",
        Seq(Fact.Bound(PredicateApplication("φ", BoundVariable(0)("x")))("x")),
        ForAll("x")(PredicateApplication("φ", BoundVariable(0)("x"))))
      val specification = axiom(
        "Specification",
        Seq(ForAll("x")(PredicateApplication("φ", BoundVariable(0)("x")))),
        PredicateApplication("φ", y))
      checkProof(
        Seq(
          ForAll("x")(PredicateApplication("φ", BoundVariable(0)("x"))),
          ForAll("x")(Implication(PredicateApplication("φ", BoundVariable(0)("x")), PredicateApplication("ψ", BoundVariable(0)("x"))))),
        Seq(
          ForAll("x")(PredicateApplication("ψ", BoundVariable(0)("x")))),
        Seq(generalization, specification, modusPonens),
        Seq(ForAll))
    }
  }
}
