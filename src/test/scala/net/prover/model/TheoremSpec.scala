package net.prover.model

import net.prover.model.Inference.{DeducedPremise, DirectPremise, RearrangementType}

class TheoremSpec extends ProverSpec {
  "theorem parser" should {
    def parseTheorem(text: String*)(implicit context: Context): Theorem = {
      Theorem.parser(context).parseAndDiscard(text.mkString("\n"))
    }
    "not prove an unfounded statement" in {
      parseTheorem(
        "Anything Is True",
        "prove φ",
        "qed")(
        contextWith()
      ) must throwAn[Exception]
    }

    val repetition = new Axiom(
      "Repeat",
      Seq(φ),
      φ)
    val deduction = new Axiom(
      "Deduction",
      Seq(DeducedPremise(φ, ψ)),
      Implication(φ, ψ))
    val modusPonens = new Axiom(
      "Modus Ponens",
      Seq(Implication(φ, ψ), φ),
      ψ)
    val implicationIsReflexive = new Axiom(
      "Implication Is Reflexive",
      Nil,
      Implication(φ, φ))
    val falseStatementsAreEquivalent = new Axiom(
      "False Statements Are Equivalent",
      Seq(Negation(φ), Negation(ψ)),
      Equivalence(φ, ψ))
    val substitutionOfEquals = new Axiom(
      "Substitution of Equals",
      Seq(Equals(x, y), SubstitutedStatementVariable(φ, x, z)),
      SubstitutedStatementVariable(φ, y, z))
    val substitutionOfEqualsReverse = new Axiom(
      "Substitution of Equals",
      Seq(Equals(x, y), SubstitutedStatementVariable(φ, y, z)),
      SubstitutedStatementVariable(φ, x, z))
    val equivalenceOfSubstitutedEquals = new Axiom(
      "Equivalence of Substituted Equals",
      Seq(Equals(x, y)),
      Equivalence(
        SubstitutedStatementVariable(φ, x, z),
        SubstitutedStatementVariable(φ, y, z)))
    val substitution = new Axiom(
      "Substitution",
      Seq(φ),
      ProvenStatement(
        SubstitutedStatementVariable(φ, y ,x),
        Conditions(Set(x), DistinctVariables.empty)))
    val generalizationWithDifferentVariables = new Axiom(
      "Generalization",
      Seq(SubstitutedStatementVariable(φ, y, x)),
      ProvenStatement(
        ForAll(x, φ),
        Conditions(Set(y), DistinctVariables(y -> φ))))
    val generalizationWithSameVariable = new Axiom(
      "Generalization",
      Seq(φ),
      ProvenStatement(
        ForAll(x, φ),
        Conditions(Set(x), DistinctVariables.empty)))
    val specification = new Axiom(
      "Specification",
      Seq(ForAll(x, φ)),
      SubstitutedStatementVariable(φ, y, x))
    val existence = new Axiom(
      "Existence",
      Seq(SubstitutedStatementVariable(φ, y, x)),
      Exists(x, φ))
    val proveExistence = new Axiom(
      "Prove Existence",
      Seq(DeducedPremise(φ, ψ), DirectPremise(Exists(x, φ))),
      ProvenStatement(ψ, Conditions(Set(x), DistinctVariables(x -> ψ))))
    val proveUniqueness = new Axiom(
      "Prove Uniqueness",
      Seq(
        DirectPremise(Exists(x, φ)),
        DeducedPremise(
          Conjunction(
            SubstitutedStatementVariable(φ, y, x),
            SubstitutedStatementVariable(φ, z, x)),
          Equals(y, z))),
      ProvenStatement(
        ExistsUnique(x, φ),
        Conditions(Set(y, z), DistinctVariables.empty)))

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
      val specification = new Axiom(
        "Specification",
        Seq(ForAll(x, φ)),
        SubstitutedStatementVariable(φ, y, x))
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
        contextWith(generalizationWithDifferentVariables))

      theorem.conclusion mustEqual ProvenStatement(
        ForAll(z, Implication(ElementOf(z, x), ElementOf(z, x))),
        Conditions(Set(y), DistinctVariables(x -> y, x -> z, y -> z)))
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
          generalizationWithSameVariable,
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
        contextWith(substitution, generalizationWithDifferentVariables))

      theorem.conclusion mustEqual ProvenStatement(
        ForAll(y, φ),
        Conditions(Set(y), DistinctVariables.empty))
    }

    "prove a statement by simplifying substitutions using distinct variable conditions" in {
      val theorem = parseTheorem(
        "XXX",
        "premise ∃ X ∀ x ↔ ∈ x X φ",
        "premise proves",
        "  ∧ ∀ x ↔ ∈ x Y φ ∀ x ↔ ∈ x Z φ",
        "  = Y Z",
        "prove ∃! X ∀ x ↔ ∈ x X φ",
        "qed")(
        contextWith(proveUniqueness))

      theorem.conclusion mustEqual ProvenStatement(
        ExistsUnique(X, ForAll(x, Equivalence(ElementOf(x, X), φ))),
        Conditions(Set(Y, Z), DistinctVariables(X -> φ, X -> x)))
    }

    "not prove a conclusion that violates an arbitrary variable condition" in {
      parseTheorem(
        "XXX",
        "assume = x y {",
        "  prove ∀ x = x y",
        "}",
        "prove → = x y ∀ x = x y",
        "qed")(
        contextWith(deduction, generalizationWithSameVariable)
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
        contextWith(generalizationWithSameVariable, specification, equivalenceOfSubstitutedEquals, deduction))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(
        Implication(
          ForAll(x, Equals(x, y)),
          ForAll(x, Equivalence(Equals(x, z), Equals(y, z)))))
    }

    "not add distinctness conditions to arbitrary variables for statement variables that are bound" in {
      val theorem = parseTheorem(
        "XXX",
        "assume ∀ x φ {",
        "  prove ∀ x ∀ x φ",
        "}",
        "prove → ∀ x φ ∀ x ∀ x φ",
        "qed")(
        contextWith(generalizationWithSameVariable, deduction))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(
        Implication(
          ForAll(x, φ),
          ForAll(x, ForAll(x, φ))))
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
        contextWith(proveExistence))

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
        Conditions(Set.empty, DistinctVariables(y -> φ, y -> z)))
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
        Conditions(Set.empty, DistinctVariables(y -> x, y -> X, y -> Y)))
    }

    "prove a statement that requires additional distinct variable conditions to be added in the resolution of a substitution in the conclusion" in {
      val axiom = Axiom(
        "XXX",
        Seq(Equals(Y, Comprehension(x, X, φ))),
        ForAll(y, Equivalence(ElementOf(y, Y), Conjunction(ElementOf(y, X), SubstitutedStatementVariable(φ, y, x)))))

      val theorem = parseTheorem(
        "XXX",
        "premise = Z comprehension x Y ∀ X → φ ∈ x X",
        "prove ∀ y ↔ ∈ y Z ∧ ∈ y Y ∀ X → φ ∈ y X",
        "qed")(
        contextWith(axiom))

      theorem.conclusion mustEqual ProvenStatement(
        ForAll(y, Equivalence(ElementOf(y, Z), Conjunction(ElementOf(y, Y), ForAll(X, Implication(φ, ElementOf(y, X)))))),
        Conditions(Set.empty, DistinctVariables(x -> φ)))
    }

    "add distinct variable conditions from a statement definition" in {
      val theorem = parseTheorem(
        "XXX",
        "premise ∃ x ∀ y ↔ ψ = y x",
        "prove ∃! y ψ",
        "qed")(
        contextWith(ExistsUnique.forwardInference.get))

      theorem.conclusion mustEqual ProvenStatement(
        ExistsUnique(y, ψ),
        Conditions(Set.empty, DistinctVariables(x -> y)))
    }

    "add distinct variables to preserve validity of a substitution" in {
      val theorem = parseTheorem(
        "XXX",
        "premise ∃ y ∈ y x",
        "premise = x ∅",
        "prove ∃ y ∈ y ∅",
        "qed")(
        contextWith(substitutionOfEquals))

      theorem.conclusion mustEqual ProvenStatement(
        Exists(y, ElementOf(y, EmptySet)),
        Conditions(Set.empty, DistinctVariables(y -> x)))
    }

    "prove an inference conclusion by simplifying a premise" in {
      val extractLeftConjunct = Axiom(
        "Extract Left Conjunct",
        Seq(Conjunction(φ, ψ)),
        φ,
        rearrangementType = RearrangementType.Simplification)
      val anythingImpliesATrueStatement = Axiom(
        "Anything Implies a True Statement",
        Seq(φ),
        Implication(ψ, φ))

      val theorem = parseTheorem(
        "XXX",
        "premise ∧ φ ψ",
        "prove → χ φ",
        "qed")(
        contextWith(extractLeftConjunct, anythingImpliesATrueStatement))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(Implication(χ, φ))
    }

    "prove a statement by rearranging" in {
      val extractLeftConjunct = Axiom(
        "Extract Left Conjunct",
        Seq(Conjunction(φ, ψ)),
        φ,
        rearrangementType = RearrangementType.Simplification)
      val combineConjunction = Axiom(
        "Combine Conjunction",
        Seq(φ, ψ),
        Conjunction(φ, ψ),
        rearrangementType = RearrangementType.Expansion)
      val addRightDisjunct = Axiom(
        "Add Left Disjunct",
        Seq(ψ),
        Disjunction(φ, ψ),
        rearrangementType = RearrangementType.Expansion)

      val theorem = parseTheorem(
        "XXX",
        "premise ∧ φ ψ",
        "premise χ",
        "prove ∨ ψ ∧ χ φ",
        "qed")(
        contextWith(extractLeftConjunct, combineConjunction, addRightDisjunct))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(Disjunction(ψ, Conjunction(χ, φ)))
    }
  }
}
