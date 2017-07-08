package net.prover.model

import net.prover.model.DetailedProof.{AssertionStep, SimplifiedReference}
import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise, RearrangementType}
import net.prover.model.components.{PlaceholderStatement, SubstitutedPlaceholderStatement, SubstitutedStatementVariable, SubstitutedTermVariable}
import net.prover.model.entries.{Axiom, Theorem}

class TheoremSpec extends ProverSpec {

  def axiom(
    premises: Seq[Premise],
    conclusion: ProvenStatement,
    rearrangementType: RearrangementType = RearrangementType.NotRearrangement
  ): Axiom = {
    Axiom("", "", "", "", "", "", premises, conclusion, rearrangementType)
  }

  "theorem parser" should {
    def parseTheorem(text: String*)(implicit context: Context): Theorem = {
      Theorem.parser(stubBook, stubChapter)(context).parseAndDiscard(text.mkString("\n"))
    }
    "not prove an unfounded statement" in {
      parseTheorem(
        "Anything Is True",
        "prove φ",
        "qed")(
        contextWith()
      ) must throwAn[Exception]
    }

    val repetition = axiom(Seq(φ), φ)
    val deduction = axiom(Seq(DeducedPremise(φ, ψ)), Implication(φ, ψ))
    val modusPonens = axiom(Seq(Implication(φ, ψ), φ), ψ)
    val implicationIsReflexive = axiom(Nil, Implication(φ, φ))
    val falseStatementsAreEquivalent = axiom(Seq(Negation(φ), Negation(ψ)), Equivalence(φ, ψ))
    val extractRightConjunct = axiom(Seq(Conjunction(φ, ψ)), ψ, RearrangementType.Simplification)
    val extractLeftConjunct = axiom(Seq(Conjunction(φ, ψ)), φ, RearrangementType.Simplification)
    val combineConjunction = axiom(Seq(φ, ψ), Conjunction(φ, ψ), RearrangementType.Expansion)
    val addRightDisjunct = axiom(Seq(ψ), Disjunction(φ, ψ), rearrangementType = RearrangementType.Expansion)
    val substitutionOfEquals = axiom(
      Seq(
        DirectPremise(Equals(x, y), isElidable = true),
        SubstitutedStatementVariable(φ, x, z)),
      SubstitutedStatementVariable(φ, y, z))
    val substitutionOfEqualsReverse = axiom(Seq(Equals(x, y), SubstitutedStatementVariable(φ, y, z)), SubstitutedStatementVariable(φ, x, z))
    val equivalenceOfSubstitutedEquals = axiom(
      Seq(Equals(x, y)),
      Equivalence(
        SubstitutedStatementVariable(φ, x, z),
        SubstitutedStatementVariable(φ, y, z)))
    val substitution = axiom(
      Seq(φ),
      ProvenStatement(
        SubstitutedStatementVariable(φ, y ,x),
        Conditions(Set(x), DistinctVariables.empty)))
    val renamedGeneralization = axiom(
      Seq(SubstitutedStatementVariable(φ, y, x)),
      ProvenStatement(
        ForAll(x, φ),
        Conditions(Set(y), DistinctVariables(y -> φ))))
    val generalization = axiom(
      Seq(φ),
      ProvenStatement(
        ForAll(x, φ),
        Conditions(Set(x), DistinctVariables.empty)),
      RearrangementType.Expansion)
    val specification = axiom(Seq(ForAll(x, φ)), SubstitutedStatementVariable(φ, y, x))
    val existence = axiom(Seq(SubstitutedStatementVariable(φ, y, x)), Exists(x, φ))
    val nameExistence = axiom(
      Seq(
        DirectPremise(Exists(x, φ), isElidable = true),
        DeducedPremise(φ, ψ)),
      ProvenStatement(
        ψ,
        Conditions(Set(x), DistinctVariables(x -> ψ))))
    val nameExistenceRenamed = axiom(
      Seq(
        DirectPremise(Exists(x, φ), isElidable = true),
        DeducedPremise(SubstitutedStatementVariable(φ, y, x), ψ)),
      ProvenStatement(
        ψ,
        Conditions(Set(x), DistinctVariables(y -> φ, y -> ψ))))

    "prove the conclusion of a premiseless inference" in {
      val theorem = parseTheorem(
        "X",
        "prove → ψ ψ",
        "qed")(
        contextWith(implicationIsReflexive))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(Implication(ψ, ψ))
    }

    "prove the conclusion of an inference whose premises match proven statements" in {
      val theorem = parseTheorem(
        "X",
        "premise → → → ψ χ ψ ψ",
        "premise → → ψ χ ψ",
        "prove ψ",
        "qed")(
        contextWith(modusPonens))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(ψ)
    }

    "not prove the conclusion of an inference whose premises can't be matched" in {
      parseTheorem(
        "X",
        "premise → φ χ",
        "premise ψ",
        "prove φ",
        "qed")(
        contextWith(modusPonens)
      ) must throwAn[Exception]
    }

    "prove the conclusion of an inference with a deduced premise that matches an assumption" in {
      val theorem = parseTheorem(
        "X",
        "premise χ",
        "assume ψ {",
        "  prove χ",
        "}",
        "prove → ψ χ",
        "qed")(
        contextWith(repetition, deduction))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(Implication(ψ, χ))
    }

    "prove a conclusion that is a substitution" in {
      val theorem = parseTheorem(
        "X",
        "premise = x y",
        "premise = x x",
        "prove = y x",
        "qed")(
        contextWith(substitutionOfEquals))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(Equals(y, x))
    }

    "prove a conclusion that is a substitution 2" in {
      val theorem = parseTheorem(
        "X",
        "premise = x y",
        "premise = z y",
        "prove = x z",
        "qed")(
        contextWith(substitutionOfEqualsReverse))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(Equals(x, z))
    }

    "prove a conclusion that is a nested substitution" in {
      val theorem = parseTheorem(
        "X",
        "premise = x y",
        "prove ↔ ∈ z x ∈ z y",
        "qed")(
        contextWith(equivalenceOfSubstitutedEquals))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(
        Equivalence(ElementOf(z, x), ElementOf(z, y)))
    }

    "prove a conclusion that is a nested substituted variable" in {
      val specification = axiom(Seq(ForAll(x, φ)), SubstitutedStatementVariable(φ, y, x))
      val theorem = parseTheorem(
        "X",
        "premise ∀ x ¬ φ",
        "prove ¬ sub y x φ",
        "qed")(
        contextWith(specification))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(
        Negation(SubstitutedStatementVariable(φ, y, x)))
    }

    "prove a conclusion with a substituted premise" in {
      val theorem = parseTheorem(
        "X",
        "premise → ∈ y x ∈ y x",
        "prove ∀ z → ∈ z x ∈ z x",
        "qed")(
        contextWith(renamedGeneralization))

      theorem.conclusion mustEqual ProvenStatement(
        ForAll(z, Implication(ElementOf(z, x), ElementOf(z, x))),
        Conditions(Set(y), DistinctVariables(y -> x, y -> z)))
    }

    "prove a conclusion with a no-op substitution" in {
      val theorem = parseTheorem(
        "X",
        "premise ∀ x φ",
        "prove φ",
        "qed")(
        contextWith(specification))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(φ)
    }

    "prove the conclusion of a transformed inference" in {
      val transform = SimpleInferenceTransform(ForAll(x, PlaceholderStatement))

      val theorem = parseTheorem(
        "X",
        "premise ∀ x ¬ ∈ x z",
        "premise ∀ x ¬ ∈ x y",
        "prove ∀ x ↔ ∈ x z ∈ x y",
        "qed")(
        contextWith(
          falseStatementsAreEquivalent,
          generalization,
          specification
        ).addInferenceTransform(transform))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(
        ForAll(x, Equivalence(ElementOf(x, z), ElementOf(x, y))))
    }

    "prove the conclusion of a partially transformed inference" in {
      val transform = PartialInferenceTransform(ForAll(x, PlaceholderStatement), SubstitutedPlaceholderStatement(y, x))

      val theorem = parseTheorem(
        "X",
        "premise ∀ y → φ ψ",
        "premise sub z y φ",
        "prove sub z y ψ",
        "qed")(
        contextWith(modusPonens, specification).addInferenceTransform(transform))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(
        SubstitutedStatementVariable(ψ, z, y))
    }

    "carry arbitrary variable condition from inference conclusion" in {
      val theorem = parseTheorem(
        "X",
        "premise φ",
        "prove sub z y φ",
        "qed")(
        contextWith(substitution))

      theorem.conclusion mustEqual ProvenStatement(
        SubstitutedStatementVariable(φ, z, y),
        Conditions(Set(y), DistinctVariables.empty))
    }

    "only prove a statement if non-distinct conditions are met" in {
      val theorem = parseTheorem(
        "X",
        "premise φ",
        "prove sub x y φ",
        "prove ∀ y φ non-distinct (y φ)",
        "qed")(
        contextWith(substitution, renamedGeneralization))

      theorem.conclusion mustEqual ProvenStatement(
        ForAll(y, φ),
        Conditions(Set(y), DistinctVariables.empty))
    }

    "prove a statement by simplifying substitutions using distinct variable conditions" in {
      val theorem = parseTheorem(
        "XXX",
        "premise φ",
        "prove ∀ x φ",
        "qed")(
        contextWith(renamedGeneralization))

      theorem.conclusion mustEqual ProvenStatement(
        ForAll(x, φ),
        Conditions(Set.empty, DistinctVariables(x -> φ)))
    }

    "not prove a conclusion that violates an arbitrary variable condition" in {
      parseTheorem(
        "XXX",
        "assume = x y {",
        "  prove ∀ x = x y",
        "}",
        "prove → = x y ∀ x = x y",
        "qed")(
        contextWith(deduction, generalization)
      ) must throwAn[Exception]
    }

    "allow arbitrary variables that only appear bound" in {
      val theorem = parseTheorem(
        "XXX",
        "assume ∀ x = x y {",
        "  prove = x y",
        "  prove ↔ = x z = y z",
        "  prove ∀ x ↔ = x z = y z",
        "}",
        "prove → ∀ x = x y ∀ x ↔ = x z = y z",
        "qed")(
        contextWith(generalization, specification, equivalenceOfSubstitutedEquals, deduction))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(
        Implication(
          ForAll(x, Equals(x, y)),
          ForAll(x, Equivalence(Equals(x, z), Equals(y, z)))))
    }

    "add distinct conditions to simplify substitutions" in {
      val theorem = parseTheorem(
        "XXX",
        "premise ∧ sub z x φ = z z",
        "prove ∃ y ∧ sub y x φ = y z",
        "qed")(
        contextWith(existence))

      theorem.conclusion mustEqual ProvenStatement(
        Exists(y, Conjunction(SubstitutedStatementVariable(φ, y, x), Equals(y, z))),
        Conditions(Set.empty, DistinctVariables(y -> φ)))
    }

    "apply distinct variable conditions to compound statements correctly" in {
      val theorem = parseTheorem(
        "XXX",
        "premise proves φ ∧ ψ sub z x χ",
        "premise ∃ x φ",
        "prove ∧ ψ sub z x χ",
        "qed")(
        contextWith(nameExistence))

      theorem.conclusion mustEqual ProvenStatement(
        Conjunction(ψ, SubstitutedStatementVariable(χ, z, x)),
        Conditions(Set(x), DistinctVariables(x -> ψ, x -> z)))
    }

    "apply distinct variable conditions to reverse a substitution" in {
      val theorem = parseTheorem(
        "XXX",
        "premise ∧ φ = x z",
        "prove ∃ y ∧ sub y x φ = y z",
        "qed")(
        contextWith(existence))

      theorem.conclusion mustEqual ProvenStatement(
        Exists(y, Conjunction(SubstitutedStatementVariable(φ, y, x), Equals(y, z))),
        Conditions(Set.empty, DistinctVariables(y -> φ)))
    }

    "prove a statement requiring a complicated resolution of a substitution" in {
      val theorem = parseTheorem(
        "XXX",
        "premise = Y comprehension x X φ",
        "premise ∀ y ↔ ∈ y comprehension x X φ ∧ ∈ y X sub y x φ",
        "prove ∀ y ↔ ∈ y Y ∧ ∈ y X sub y x φ",
        "qed")(
        contextWith(substitutionOfEqualsReverse))

      theorem.conclusion mustEqual ProvenStatement(
        ForAll(y, Equivalence(ElementOf(y, Y), Conjunction(ElementOf(y, X), SubstitutedStatementVariable(φ, y, x)))),
        Conditions(Set.empty, DistinctVariables(y -> x)))
    }

    "prove a statement that requires additional distinct variable conditions to be added in the resolution of a substitution in the conclusion" in {
      val elementOfComprehension = axiom(
        Seq(Equals(Y, Comprehension(x, X, φ))),
        ForAll(y, Equivalence(ElementOf(y, Y), Conjunction(ElementOf(y, X), SubstitutedStatementVariable(φ, y, x)))))

      val theorem = parseTheorem(
        "XXX",
        "premise = Z comprehension x Y ∀ X → φ ∈ x X",
        "prove ∀ y ↔ ∈ y Z ∧ ∈ y Y ∀ X → φ ∈ y X",
        "qed")(
        contextWith(elementOfComprehension))

      theorem.conclusion mustEqual ProvenStatement(
        ForAll(y, Equivalence(ElementOf(y, Z), Conjunction(ElementOf(y, Y), ForAll(X, Implication(φ, ElementOf(y, X)))))),
        Conditions(Set.empty, DistinctVariables(x -> φ, x -> X)))
    }

    "add distinct variables to preserve validity of a substitution" in {
      val theorem = parseTheorem(
        "XXX",
        "premise ∀ x = x y",
        "prove = z y",
        "qed")(
        contextWith(specification))

      theorem.conclusion mustEqual ProvenStatement(
        Equals(z, y),
        Conditions(Set.empty, DistinctVariables(x -> y)))
    }

    "prove an inference conclusion by simplifying a premise" in {
      val anythingImpliesATrueStatement = axiom(
        Seq(φ),
        Implication(ψ, φ))

      val theorem = parseTheorem(
        "XXX",
        "premise ∧ φ ψ",
        "prove → χ φ",
        "qed")(
        contextWith(extractLeftConjunct, anythingImpliesATrueStatement))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(Implication(χ, φ))
      theorem.proof.steps.last.asInstanceOf[AssertionStep].references.head.asInstanceOf[SimplifiedReference].html mustEqual "φ"
    }

    "prove a statement by rearranging" in {

      val theorem = parseTheorem(
        "XXX",
        "premise ∧ φ ψ",
        "premise χ",
        "prove ∨ ψ ∧ χ φ",
        "qed")(
        contextWith(extractLeftConjunct, combineConjunction, addRightDisjunct))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(Disjunction(ψ, Conjunction(χ, φ)))
    }

    "add conditions when proving a statement by rearranging " in {
      val addNegatedConjunct = axiom(
        Seq(Negation(φ)),
        Negation(Conjunction(φ, ψ)),
        RearrangementType.Expansion)

      val theorem = parseTheorem(
        "XXX",
        "premise ¬ = x y",
        "prove ∀ x ¬ ∧ = x y φ",
        "qed")(
        contextWith(addNegatedConjunct, generalization))

      theorem.conclusion mustEqual ProvenStatement(
        ForAll(x, Negation(Conjunction(Equals(x, y), φ))),
        Conditions(Set(x), DistinctVariables.empty))
    }

    "resolve substitutions involving substituted term variables" in {
      val theorem = parseTheorem(
        "XXX",
        "premise = x y",
        "premise = sub x a z sub x a z",
        "prove = sub x a z sub y a z",
        "qed")(
        contextWith(substitutionOfEquals))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(
        Equals(SubstitutedTermVariable(z, x, a), SubstitutedTermVariable(z, y, a)))
    }

    "prove a naming step using an elided premise" in {
      val elementOfUnion = axiom(
        Seq(ElementOf(a, Union(x))),
        Exists(y, Conjunction(ElementOf(a, y), ElementOf(y, x))))
      val theorem = parseTheorem(
        "XXX",
        "premise ∈ n union y",
        "let x ∧ ∈ n x ∈ x y {",
        "  prove ∈ x y",
        "  prove ∃ z ∈ z y",
        "}",
        "qed")(
        contextWith(elementOfUnion, nameExistence, existence, extractRightConjunct))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(
        Exists(z, ElementOf(z, y)))
    }

    "prove an elided premise using a double substitution" in {
      val elementOfComprehension = axiom(
        Seq(Equals(X, Comprehension(y, Y, φ)), ElementOf(x, X)),
        SubstitutedStatementVariable(φ, x, y))

      val theorem = parseTheorem(
        "XXX",
        "premise = Z comprehension x X ∃ y ∧ ∈ y x = x x",
        "premise ∈ z Z",
        "let a ∧ ∈ a z = z z {",
        "  prove ∈ a z",
        "  prove ∃ n ∈ n z",
        "}",
        "qed")(
        contextWith(elementOfComprehension, extractLeftConjunct, existence, nameExistenceRenamed))

      theorem.conclusion mustEqual ProvenStatement(
        Exists(n, ElementOf(n, z)),
        Conditions(Set.empty, DistinctVariables(x -> y, y -> z)))
    }

    "prove an elided premise by working out substitution targets" in {
      val pairIsSymmetric = axiom(
        Nil,
        Equals(Pair(x, y), Pair(y, x)))

      val theorem = parseTheorem(
        "XXX",
        "premise = x pair y z ",
        "prove = x pair z y",
        "qed")(
        contextWith(pairIsSymmetric, substitutionOfEquals))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(Equals(x, Pair(z, y)))
    }
  }
}
