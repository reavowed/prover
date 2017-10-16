package net.prover.model

import java.nio.file.Paths

import net.prover.model.Inference.RearrangementType
import net.prover.model.expressions._
import net.prover.model.entries.{Axiom, StatementDefinition}
import net.prover.model.proof._

class TheoremSpec extends ProverSpec {

  def axiom(
    name: String,
    premises: Seq[PremiseMagnet],
    conclusion: Statement,
    rearrangementType: RearrangementType = RearrangementType.NotRearrangement
  ): Axiom = {
    Axiom(name, name.formatAsKey, "test-chapter", "Test Chapter", "test-book", "Test Book", premises, conclusion, rearrangementType)
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

    def prove(
      premises: Seq[PremiseMagnet],
      proofSteps: Seq[StepOutline],
      inferences: Seq[Inference],
      transformations: Seq[StatementDefinition] = Nil
    ): Proof = {
      Proof.fillInOutline(
        premises,
        ProofOutline(proofSteps),
        inferences,
        Nil,
        transformations)
    }

    def checkProof(
      premises: Seq[PremiseMagnet],
      proofSteps: Seq[StepOutline],
      inferences: Seq[Inference],
      transformations: Seq[StatementDefinition] = Nil
    ) = {
      val proof = prove(premises, proofSteps, inferences, transformations)
//      proof.conclusion mustEqual proofSteps.ofType[StepOutline.WithAssertion].last.innermostAssertionStep.assertion
      val cachedProof = CachedProof(Paths.get(""), premises, proof.steps.map(_.cached))
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
    val deduction = axiom("Deduction", Seq(Fact.Deduced(φ, ψ)), Implication(φ, ψ), RearrangementType.Contraction)
    val modusPonens = axiom("Modus Ponens", Seq(Implication(φ, ψ), φ), ψ)
    val implicationIsReflexive = axiom("Implication Is Reflexive", Nil, Implication(φ, φ))
    val extractLeftConjunct = axiom("Extract Left Conjunct", Seq(Conjunction(φ, ψ)), φ, RearrangementType.Simplification)
    val extractRightConjunct = axiom("Extract Left Conjunct", Seq(Conjunction(φ, ψ)), ψ, RearrangementType.Simplification)
    val combineConjunction = axiom("Combine Conjunction", Seq(φ, ψ), Conjunction(φ, ψ), RearrangementType.Expansion)
    val addRightDisjunct = axiom("Add Right Disjunct", Seq(ψ), Disjunction(φ, ψ), RearrangementType.Expansion)

    val generalization = axiom(
      "Generalization",
      Seq(Fact.ScopedVariable(φ.!(FunctionParameter("x", 0)))("x").elidable),
      ForAll("x")(φ.!(FunctionParameter("x", 0))),
      RearrangementType.Contraction)
    val specification = axiom(
      "Specification",
      Seq(ForAll("x")(φ.!(FunctionParameter("x", 0)))),
      φ(a))
    val substitutionOfEquals = axiom(
      "Substitution of Equals",
      Seq(Equals(a, b).elidable, φ(a)),
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

    "prove a conclusion using a contracted premise" in {
      checkProof(
        Seq(
          Fact.ScopedVariable(Implication.!(
            ElementOf.!(FunctionParameter("x", 0), a.^),
            ElementOf.!(FunctionParameter("x", 0), a.^)))(
            "x")),
        Seq(Subset(a, a)),
        Seq(generalization, Subset.inferences.head))
    }

    "prove a conclusion using a multiply-contracted premise" in {
      checkProof(
        Seq(
          Fact.ScopedVariable(
            Fact.Deduced(
              ElementOf.!(FunctionParameter("x", 0), a.^),
              ElementOf.!(FunctionParameter("x", 0), a.^)))(
            "x")),
        Seq(Subset(a, a)),
        Seq(deduction, generalization, Subset.inferences.head))
    }

    "apply a transformation with applied and unapplied statements" in {
      val modusTollens = axiom("Modus Tollens", Seq(Implication(φ, ψ), Negation(ψ)), Negation(φ))
      checkProof(
        Seq(
          ForAll("x")(Implication.!(φ.!(FunctionParameter("x", 0)), ψ.^)),
          Negation(ψ)),
        Seq(
          StepOutline.ScopedVariable("x", Seq(Negation.!(φ.!(FunctionParameter("x", 0))))),
          ForAll("x")(Negation.!(φ.!(FunctionParameter("x", 0))))),
        Seq(specification, generalization, modusTollens),
        Seq(ForAll))
    }

    "prove a conclusion using a sub-contraction" in {
      checkProof(
        Seq(
          Fact.ScopedVariable(Fact.ScopedVariable(φ.^^)("y"))("x")),
        Seq(ForAll("x")(ForAll.!("y")(φ.^^))),
        Seq(generalization))
    }

    "prove a naming step" in {
      val valueForExistence = axiom(
        "Value for Existence",
        Seq(
          Exists("x")(φ.!(FunctionParameter("x", 0))),
          ForAll("x")(Implication.!(φ.!(FunctionParameter("x", 0)), ψ.^))),
        ψ)
      checkProof(
        Seq(
          Exists("x")(Conjunction.!(φ.!(FunctionParameter("x", 0)), ψ.^)),
          ForAll("x")(φ.!(FunctionParameter("x", 0)))),
        Seq(StepOutline.Naming(
          "a",
          Conjunction.!(φ.!(FunctionParameter("a", 0)), ψ.^),
          Seq(ψ.^))),
        Seq(extractRightConjunct, valueForExistence, deduction, generalization),
        Seq(ForAll))
    }

    "prove with an elided premise" in {
      val repeatedPairIsSingleton = axiom(
        "Repeated Pair Is Singleton",
        Nil,
        Equals(Pair(a, a), Singleton(a)))
      checkProof(
        Seq(Equals(b, Pair(a,a))),
        Seq(Equals(b, Singleton(a))),
        Seq(repeatedPairIsSingleton, substitutionOfEquals))
    }
  }
}
