package net.prover.model

import net.prover.model.Inference.{DeducedPremise, DirectPremise}

class TheoremSpec extends ProverSpec {
  "theorem parser" should {
    def parseTheorem(text: String*)(implicit context: Context): Theorem = {
      Theorem.parser(context).parseAndDiscard(text.mkString("\n"))
    }
    "not prove an unfounded statement" in {
      parseTheorem(
        "Anything Is True",
        "prove φ",
        "qed"
      ) must throwAn[Exception]
    }

    "prove the conclusion of a premiseless inference" in {
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

    "prove the conclusion of an inference whose premises match proven statements" in {
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

    "not prove the conclusion of an inference whose premises can't be matched" in {
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

    "prove the conclusion of an inference with a deduced premise that matches an assumption" in {
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

    "prove a conclusion that is a substitution" in {
      val substitutionOfEquals = new Axiom(
        "Substitution of Equals",
        Seq(Equals(x, y), SubstitutedStatementVariable(φ, x, z)),
        SubstitutedStatementVariable(φ, y, z))

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
      val substitutionOfEquals = new Axiom(
        "Substitution of Equals",
        Seq(Equals(x, y), SubstitutedStatementVariable(φ, y, z)),
        SubstitutedStatementVariable(φ, x, z))

      val theorem = parseTheorem(
        "X",
        "premise = x y",
        "premise = z y",
        "prove = x z",
        "qed")(
        contextWith(substitutionOfEquals))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(Equals(x, z))
    }

    "prove a conclusion that is a nested substitution" in {
      val equivalenceOfSubstitutedEqauls = new Axiom(
        "Equivalence of Substituted Equals",
        Seq(Equals(x, y)),
        Equivalence(
          SubstitutedStatementVariable(φ, x, z),
          SubstitutedStatementVariable(φ, y, z)))

      val theorem = parseTheorem(
        "X",
        "premise = x y",
        "prove ↔ ∈ z x ∈ z y",
        "qed")(
        contextWith(equivalenceOfSubstitutedEqauls))

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
      val generalization = new Axiom(
        "Generalization",
        Seq(SubstitutedStatementVariable(φ, y, x)),
        ForAll(x, φ))

      val theorem = parseTheorem(
        "X",
        "premise → ∈ y x ∈ y x",
        "prove ∀ z → ∈ z x ∈ z x",
        "qed")(
        contextWith(generalization))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(
        ForAll(z, Implication(ElementOf(z, x), ElementOf(z, x))))
    }

    "prove a conclusion with a no-op substitution" in {
      val specification = new Axiom(
        "Specification",
        Seq(ForAll(x, φ)),
        SubstitutedStatementVariable(φ, y, x))

      val theorem = parseTheorem(
        "X",
        "premise ∀ x φ",
        "prove φ",
        "qed")(
        contextWith(specification))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(φ)
    }

    "prove the conclusion of a transformed inference" in {
      val falseStatementsAreEquivalent = new Axiom(
        "False Statements Are Equivalent",
        Seq(Negation(φ), Negation(ψ)),
        Equivalence(φ, ψ))
      val generalization = new Axiom(
        "Generalization",
        Seq(φ),
        ProvenStatement(
          ForAll(x, φ),
          Conditions(Set(x), Map.empty)))
      val specification = new Axiom(
        "Specification",
        Seq(ForAll(x, φ)),
        SubstitutedStatementVariable(φ, y, x))
      val transform = new InferenceTransform(ForAll(x, PlaceholderStatement))

      val theorem = parseTheorem(
        "X",
        "premise ∀ x ¬ ∈ x z",
        "premise ∀ x ¬ ∈ x y",
        "prove ∀ x ↔ ∈ x z ∈ x y",
        "qed")(
        contextWith(falseStatementsAreEquivalent, generalization, specification).addInferenceTransform(transform))

      theorem.conclusion mustEqual ProvenStatement.withNoConditions(
        ForAll(x, Equivalence(ElementOf(x, z), ElementOf(x, y))))
    }

    "carry arbitrary variable condition from inference conclusion" in {
      val substitution = new Axiom(
        "Substitution",
        Seq(φ),
        ProvenStatement(
          SubstitutedStatementVariable(φ, y ,x),
          Conditions(arbitraryVariables = Set(x), distinctVariables = Map.empty)))

      val theorem = parseTheorem(
        "X",
        "premise φ",
        "prove sub z y φ",
        "qed")(
        contextWith(substitution))

      theorem.conclusion mustEqual ProvenStatement(
        SubstitutedStatementVariable(φ, z, y),
        Conditions(Set(y), Map.empty))
    }

    "only prove a statement if the conditions match when specified explicitly" in {
      val substitution = new Axiom(
        "Substitution",
        Seq(φ),
        ProvenStatement(
          SubstitutedStatementVariable(φ, y ,x),
          Conditions(arbitraryVariables = Set(x), distinctVariables = Map.empty)))
      val generalization = new Axiom(
        "Generalization",
        Seq(SubstitutedStatementVariable(φ, y, x)),
        ProvenStatement(
          ForAll(x, φ),
          Conditions(Set(y), Map(y -> Variables(Set(φ), Set.empty)))))

      val theorem = parseTheorem(
        "X",
        "premise φ",
        "prove sub x y φ",
        "prove ∀ y φ arbitrary-variables(y) distinct-variables ()",
        "qed")(
        contextWith(substitution, generalization))

      theorem.conclusion mustEqual ProvenStatement(
        ForAll(y, φ),
        Conditions(Set(y), Map.empty))
    }

    "prove a statement by simplifying substitutions using distinct variable conditions" in {
      val proveExistence = new Axiom(
        "Prove Existence",
        Seq(
          DirectPremise(Exists(x, φ)),
          DeducedPremise(
            Conjunction(
              SubstitutedStatementVariable(φ, y, x),
              SubstitutedStatementVariable(φ, z, x)),
            Equals(y, z))),
        ProvenStatement(
          ExistsUnique(x, φ),
          Conditions(Set(y, z), Map.empty)))

      val theorem = parseTheorem(
        "XXX",
        "premise ∃ X ∀ x ↔ ∈ x X φ",
        "premise proves",
        "  ∧ ∀ x ↔ ∈ x Y φ ∀ x ↔ ∈ x Z φ",
        "  = Y Z",
        "prove ∃! X ∀ x ↔ ∈ x X φ",
        "qed")(
        contextWith(proveExistence))

      theorem.conclusion mustEqual ProvenStatement(
        ExistsUnique(X, ForAll(x, Equivalence(ElementOf(x, X), φ))),
        Conditions(Set(Y, Z), Map(X -> Variables(Set(φ), Set.empty))))
    }
  }
}
