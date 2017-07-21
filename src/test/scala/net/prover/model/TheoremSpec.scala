package net.prover.model

import java.nio.file.Paths

import net.prover.model.Proof.SimplificationReference
import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise, RearrangementType}
import net.prover.model.ProofOutline.{AssertionStep, AssumptionStep, NamingStep}
import net.prover.model.components._
import net.prover.model.entries.Axiom

class TheoremSpec extends ProverSpec {

  def axiom(
    name: String,
    premises: Seq[Premise],
    conclusion: ProvenStatement,
    rearrangementType: RearrangementType = RearrangementType.NotRearrangement
  ): Axiom = {
    Axiom(name, name.formatAsKey, "test-chapter", "Test Chapter", "test-book", "Test Book", premises, conclusion, rearrangementType)
  }

  "theorem parser" should {
    implicit def assertionStepFromStatement(statement: Statement): ProofOutline.Step = {
      AssertionStep(statement, location = ProofOutline.Location("test.book", 1))
    }
    def assertionStepWithNonDistinctVariables(statement: Statement, nonDistinctVariables: Set[(TermVariable, Variable)]): ProofOutline.Step = {
      AssertionStep(
        statement,
        nonDistinctVariables = nonDistinctVariables,
        location = ProofOutline.Location("test.book", 1))
    }

    implicit def assumptionStepFromStatementPair(tuple: (Statement, Statement)): ProofOutline.Step = {
      AssumptionStep(tuple._1, Seq(assertionStepFromStatement(tuple._2)))
    }

    implicit def assumptionStepFromStatementAndStatements(tuple: (Statement, Seq[Statement])): ProofOutline.Step = {
      AssumptionStep(tuple._1, tuple._2.map(assertionStepFromStatement))
    }

    def prove(
      premises: Seq[Premise],
      proofSteps: Seq[ProofOutline.Step],
      inferences: Seq[Inference],
      inferenceTransforms: Seq[InferenceTransform]
    ): Proof = {
      Proof.fillInOutline(
        premises,
        ProofOutline(proofSteps),
        inferences,
        inferenceTransforms)
    }

    def checkProof(
      premises: Seq[Premise],
      proofSteps: Seq[ProofOutline.Step],
      inferences: Seq[Inference],
      inferenceTransforms: Seq[InferenceTransform],
      conditions: Conditions = Conditions.empty
    ) = {
      val proof = prove(premises, proofSteps, inferences, inferenceTransforms)
      proof.conclusion mustEqual ProvenStatement(
        proofSteps.ofType[ProofOutline.StepWithAssertion].last.innermostAssertionStep.assertion,
        conditions)
      proof.matchesOutline(ProofOutline(proofSteps)) must beTrue
      val serializedProof = proof.serialized
      val deserializedProof = Proof.parser.parse(Tokenizer.fromString(serializedProof, Paths.get("")))._1
      CachedProof(Paths.get(""), premises, deserializedProof).validate(inferences, inferenceTransforms) must beSome(proof)
    }

    "not prove an unfounded statement" in {
      prove(Nil, Seq(φ), Nil, Nil) must throwAn[Exception]
    }

    val repetition = axiom("Repetition", Seq(φ), φ)
    val deduction = axiom("Deduction", Seq(DeducedPremise(φ, ψ)), Implication(φ, ψ))
    val modusPonens = axiom("Modus Ponens", Seq(Implication(φ, ψ), φ), ψ)
    val implicationIsReflexive = axiom("Implication Is Reflexive", Nil, Implication(φ, φ))
    val falseStatementsAreEquivalent = axiom("False Statements Are Equivalent", Seq(Negation(φ), Negation(ψ)), Equivalence(φ, ψ))
    val extractRightConjunct = axiom("Extract Right Conjunct", Seq(Conjunction(φ, ψ)), ψ, RearrangementType.Simplification)
    val extractLeftConjunct = axiom("Extract Left Conjunct", Seq(Conjunction(φ, ψ)), φ, RearrangementType.Simplification)
    val combineConjunction = axiom("Combine Conjunction", Seq(φ, ψ), Conjunction(φ, ψ), RearrangementType.Expansion)
    val addRightDisjunct = axiom("Add Right Disjunct", Seq(ψ), Disjunction(φ, ψ), rearrangementType = RearrangementType.Expansion)
    val substitutionOfEquals = axiom(
      "Substitution of Equals",
      Seq(
        DirectPremise(Equals(x, y), isElidable = true),
        φ.sub(x, z)),
      φ.sub(y, z))
    val substitutionOfEqualsReverse = axiom(
      "Substitution of Equals",
      Seq(Equals(x, y), φ.sub(y, z)),
      φ.sub(x, z))
    val equivalenceOfSubstitutedEquals = axiom(
      "Equivalence of Substituted Equals",
      Seq(Equals(x, y)),
      Equivalence(
        φ.sub(x, z),
        φ.sub(y, z)))
    val substitution = axiom(
      "Substitution",
      Seq(φ),
      ProvenStatement(
        φ.sub(y, x),
        Conditions(Set(x), DistinctVariables.empty)))
    val renamedGeneralization = axiom(
      "Renamed Generalization",
      Seq(φ.sub(y, x)),
      ProvenStatement(
        ForAll(x, φ),
        Conditions(Set(y), DistinctVariables(y -> φ))))
    val generalization = axiom(
      "Generalization",
      Seq(φ),
      ProvenStatement(
        ForAll(x, φ),
        Conditions(Set(x), DistinctVariables.empty)),
      RearrangementType.Expansion)
    val specification = axiom("Specification", Seq(ForAll(x, φ)), φ.sub(y, x))
    val existence = axiom("Existence", Seq(φ.sub(y, x)), Exists(x, φ))
    val nameExistence = axiom(
      "Name Existence",
      Seq(
        DirectPremise(Exists(x, φ), isElidable = true),
        DeducedPremise(φ, ψ)),
      ProvenStatement(
        ψ,
        Conditions(Set(x), DistinctVariables(x -> ψ))))
    val nameExistenceRenamed = axiom(
      "Name Existence (Renamed)",
      Seq(
        DirectPremise(Exists(x, φ), isElidable = true),
        DeducedPremise(φ.sub(y, x), ψ)),
      ProvenStatement(
        ψ,
        Conditions(Set(x), DistinctVariables(y -> φ, y -> ψ))))

    "prove the conclusion of a premiseless inference" in {
      checkProof(
        Nil,
        Seq(Implication(ψ, ψ)),
        Seq(implicationIsReflexive),
        Nil)
    }

    "prove the conclusion of an inference whose premises match proven statements" in {
      checkProof(
        Seq(Implication(Implication(Implication(ψ, χ), ψ), ψ), Implication(Implication(ψ, χ), ψ)),
        Seq(ψ),
        Seq(modusPonens),
        Nil)
    }

    "not prove the conclusion of an inference whose premises can't be matched" in {
      prove(
        Seq(Implication(φ, χ), ψ),
        Seq(φ),
        Seq(modusPonens),
        Nil
      ) must throwAn[Exception]
    }

    "prove the conclusion of an inference with a deduced premise that matches an assumption" in {
      checkProof(
        Seq(χ),
        Seq(ψ -> χ, Implication(ψ, χ)),
        Seq(repetition, deduction),
        Nil)
    }

    "prove a conclusion that is a substitution" in {
      checkProof(
        Seq(Equals(x, y), Equals(x, x)),
        Seq(Equals(y, x)),
        Seq(substitutionOfEquals),
        Nil)
    }

    "prove a conclusion that is a substitution 2" in {
      checkProof(
        Seq(Equals(x, y), Equals(z, y)),
        Seq(Equals(x, z)),
        Seq(substitutionOfEqualsReverse),
        Nil)
    }

    "prove a conclusion that is a nested substitution" in {
      checkProof(
        Seq(Equals(x, y)),
        Seq(Equivalence(ElementOf(z, x), ElementOf(z, y))),
        Seq(equivalenceOfSubstitutedEquals),
        Nil)
    }

    "prove a conclusion that is a nested substituted variable" in {
      checkProof(
        Seq(ForAll(x, Negation(φ))),
        Seq(Negation(φ.sub(y, x))),
        Seq(specification),
        Nil)
    }

    "prove a conclusion with a substituted premise" in {
      checkProof(
        Seq(Implication(ElementOf(y, x), ElementOf(y, x))),
        Seq(ForAll(z, Implication(ElementOf(z, x), ElementOf(z, x)))),
        Seq(renamedGeneralization),
        Nil,
        Conditions(Set(y), DistinctVariables(y -> x, y -> z)))
    }

    "prove a conclusion with a no-op substitution" in {
      checkProof(
        Seq(ForAll(x, φ)),
        Seq(φ),
        Seq(specification),
        Nil)
    }

    "prove the conclusion of a transformed inference" in {
      val transform = SimpleInferenceTransform(ForAll(x, PlaceholderStatement))
      checkProof(
        Seq(ForAll(x, Negation(ElementOf(x, z))), ForAll(x, Negation(ElementOf(x, y)))),
        Seq(ForAll(x, Equivalence(ElementOf(x, z), ElementOf(x, y)))),
        Seq(falseStatementsAreEquivalent, generalization, specification),
        Seq(transform))
    }

    "prove the conclusion of a partially transformed inference" in {
      val transform = PartialInferenceTransform(ForAll(x, PlaceholderStatement), SubstitutedPlaceholderStatement(y, x))
      checkProof(
        Seq(ForAll(y, Implication(φ, ψ)), φ.sub(z, y)),
        Seq(ψ.sub(z, y)),
        Seq(modusPonens, specification),
        Seq(transform))
    }

    "carry arbitrary variable condition from inference conclusion" in {
      checkProof(
        Seq(φ),
        Seq(φ.sub(z, y)),
        Seq(substitution),
        Nil,
        Conditions(Set(y), DistinctVariables.empty))
    }

    "only prove a statement if non-distinct conditions are met" in {
      checkProof(
        Seq(φ),
        Seq(
          assertionStepFromStatement(φ.sub(x, y)),
          assertionStepWithNonDistinctVariables(ForAll(y, φ), Set(y -> φ))),
        Seq(substitution, renamedGeneralization),
        Nil,
        Conditions(Set(y), DistinctVariables.empty))
    }

    "prove a statement by simplifying substitutions using distinct variable conditions" in {
      checkProof(
        Seq(φ),
        Seq(ForAll(x, φ)),
        Seq(renamedGeneralization),
        Nil,
        Conditions(Set.empty, DistinctVariables(x -> φ)))
    }

    "not prove a conclusion that violates an arbitrary variable condition" in {
      prove(
        Nil,
        Seq(
          Equals(x, y) -> ForAll(x, Equals(x, y)),
          Implication(Equals(x, y), ForAll(x, Equals(x, y)))),
        Seq(deduction, generalization),
        Nil
      ) must throwAn[Exception]
    }

    "allow arbitrary variables that only appear bound" in {
      checkProof(
        Nil,
        Seq(
          ForAll(x, Equals(x, y)) -> Seq(
            Equals(x, y),
            Equivalence(Equals(x, z), Equals(y, z)),
            ForAll(x, Equivalence(Equals(x, z), Equals(y, z)))),
          Implication(ForAll(x, Equals(x, y)), ForAll(x, Equivalence(Equals(x, z), Equals(y, z))))),
        Seq(generalization, specification, equivalenceOfSubstitutedEquals, deduction),
        Nil)
    }

    "add distinct conditions to simplify substitutions" in {
      checkProof(
        Seq(Conjunction(φ.sub(z, x), Equals(z, z))),
        Seq(Exists(y, Conjunction(φ.sub(y, x), Equals(y, z)))),
        Seq(existence),
        Nil,
        Conditions(Set.empty, DistinctVariables(y -> φ)))
    }

    "apply distinct variable conditions to compound statements correctly" in {
      checkProof(
        Seq(φ -> Conjunction(ψ, χ.sub(z, x)), Exists(x, φ)),
        Seq(Conjunction(ψ, χ.sub(z, x))),
        Seq(nameExistence),
        Nil,
        Conditions(Set(x), DistinctVariables(x -> ψ, x -> z)))
    }

    "apply distinct variable conditions to reverse a substitution" in {
      checkProof(
        Seq(Conjunction(φ, Equals(x, z))),
        Seq(Exists(y, Conjunction(φ.sub(y, x), Equals(y, z)))),
        Seq(existence),
        Nil,
        Conditions(Set.empty, DistinctVariables(y -> φ)))
    }

    "prove a statement requiring a complicated resolution of a substitution" in {
      checkProof(
        Seq(
          Equals(Y, Comprehension(x, X, φ)),
          ForAll(y, Equivalence(
            ElementOf(y, Comprehension(x, X, φ)),
            Conjunction(ElementOf(y, X), φ.sub(y, x))))),
        Seq(ForAll(y, Equivalence(ElementOf(y, Y), Conjunction(ElementOf(y, X), φ.sub(y, x))))),
        Seq(substitutionOfEqualsReverse),
        Nil,
        Conditions(Set.empty, DistinctVariables(y -> x)))
    }

    "prove a statement that requires additional distinct variable conditions to be added in the resolution of a substitution in the conclusion" in {
      val elementOfComprehension = axiom(
        "Element of Comprehension",
        Seq(Equals(Y, Comprehension(x, X, φ))),
        ForAll(y, Equivalence(ElementOf(y, Y), Conjunction(ElementOf(y, X), φ.sub(y, x)))))

      checkProof(
        Seq(Equals(Z, Comprehension(x, Y, ForAll(X, Implication(φ, ElementOf(x, X)))))),
        Seq(ForAll(y, Equivalence(ElementOf(y, Z), Conjunction(ElementOf(y, Y), ForAll(X, Implication(φ, ElementOf(y, X))))))),
        Seq(elementOfComprehension),
        Nil,
        Conditions(Set.empty, DistinctVariables(x -> φ, x -> X)))
    }

    "add distinct variables to preserve validity of a substitution" in {
      checkProof(
        Seq(ForAll(x, Equals(x, y))),
        Seq(Equals(z, y)),
        Seq(specification),
        Nil,
        Conditions(Set.empty, DistinctVariables(x -> y)))
    }

    "prove an inference conclusion by simplifying a premise" in {
      val anythingImpliesATrueStatement = axiom(
        "Anything Implies a True Statement",
        Seq(φ),
        Implication(ψ, φ))

      val proof = prove(
        Seq(Conjunction(φ, ψ)),
        Seq(Implication(χ, φ)),
        Seq(extractLeftConjunct, anythingImpliesATrueStatement),
        Nil)
      proof.conclusion mustEqual ProvenStatement.withNoConditions(Implication(χ, φ))
      proof.steps.last.asInstanceOf[Proof.AssertionStep].references.head.asInstanceOf[SimplificationReference].statement mustEqual φ
    }

    "prove a statement by rearranging" in {
      checkProof(
        Seq(Conjunction(φ, ψ), χ),
        Seq(Disjunction(ψ, Conjunction(χ, φ))),
        Seq(extractLeftConjunct, combineConjunction, addRightDisjunct),
        Nil)
    }

    "add conditions when proving a statement by rearranging " in {
      val addNegatedConjunct = axiom(
        "Add Negated Conjunct",
        Seq(Negation(φ)),
        Negation(Conjunction(φ, ψ)),
        RearrangementType.Expansion)

      checkProof(
        Seq(Negation(Equals(x, y))),
        Seq(ForAll(x, Negation(Conjunction(Equals(x, y), φ)))),
        Seq(addNegatedConjunct, generalization),
        Nil,
        Conditions(Set(x), DistinctVariables.empty))
    }

    "resolve substitutions involving substituted term variables" in {
      checkProof(
        Seq(Equals(x, y), Equals(SubstitutedTermVariable(z, x, a), SubstitutedTermVariable(z, x, a))),
        Seq(Equals(SubstitutedTermVariable(z, x, a), SubstitutedTermVariable(z, y, a))),
        Seq(substitutionOfEquals),
        Nil)
    }

    "prove a naming step using an elided premise" in {
      val elementOfUnion = axiom(
        "Element of Union",
        Seq(ElementOf(a, Union(x))),
        Exists(y, Conjunction(ElementOf(a, y), ElementOf(y, x))))
      checkProof(
        Seq(ElementOf(n, Union(y))),
        Seq(NamingStep(x, Conjunction(ElementOf(n, x), ElementOf(x, y)), Seq(
          ElementOf(x, y),
          Exists(z, ElementOf(z, y))))),
        Seq(elementOfUnion, nameExistence, existence, extractRightConjunct),
        Nil)
    }

    "prove an elided premise using a double substitution" in {
      val elementOfComprehension = axiom(
        "Element of Comprehension",
        Seq(Equals(X, Comprehension(y, Y, φ)), ElementOf(x, X)),
        φ.sub(x, y))

      checkProof(
        Seq(Equals(Z, Comprehension(x, X, Exists(y, Conjunction(ElementOf(y, x), Equals(x, x))))), ElementOf(z, Z)),
        Seq(NamingStep(a, Conjunction(ElementOf(a, z), Equals(z, z)), Seq(
          ElementOf(a, z),
          Exists(n, ElementOf(n, z))))),
        Seq(elementOfComprehension, extractLeftConjunct, existence, nameExistenceRenamed),
        Nil,
        Conditions(Set.empty, DistinctVariables(x -> y, y -> z)))
    }

    "prove an elided premise by working out substitution targets" in {
      val pairIsSymmetric = axiom(
        "Pair Is Symmetric",
        Nil,
        Equals(Pair(x, y), Pair(y, x)))

      checkProof(
        Seq(Equals(x, Pair(y, z))),
        Seq(Equals(x, Pair(z, y))),
        Seq(pairIsSymmetric, substitutionOfEquals),
        Nil)
    }
  }
}
