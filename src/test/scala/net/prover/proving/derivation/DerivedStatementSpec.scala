package net.prover.proving.derivation

import net.prover.{ContextHelper, StepHelpers}
import net.prover.model.*
import net.prover.model.TestDefinitions.{a, b, *}
import net.prover.model.expressions.{Statement, Term}
import net.prover.model.proof.{Step, SubstitutionContext}
import net.prover.proving.extraction.{AppliedExtraction, AppliedInferenceExtraction}
import org.specs2.mutable.Specification

class DerivedStatementSpec extends Specification with StepHelpers with ContextHelper {
  given SubstitutionContext = SubstitutionContext.outsideProof
  given availableEntries: AvailableEntries = defaultAvailableEntries

  def step(inference: Inference, statements: Seq[Statement], terms: Seq[Term]): Step.AssertionStep = {
    assertion(inference, statements, terms)(summon)
  }
  def applied(inference: Inference, terms: Seq[Term]): AppliedInferenceExtraction = {
    AppliedInferenceExtraction(step(inference, Nil, terms), AppliedExtraction(Nil, Nil))
  }

  val known = DerivedStatement.Known(Equals(Product(a, b), Product(c, d)))
  val otherKnown = DerivedStatement.Known(φ)

  // Substantive
  val simplification = applied(firstCoordinateOfOrderedPairInCartesianProduct, Seq(a, b, c, d))
  val otherSimplification = applied(firstCoordinateOfElementOfCartesianProduct, Seq(a, b, c))
  val functionApplication = step(functionApplicationIsElementOfRange, Nil, Seq(c, d))
  // Simplification
  val singleInferenceExtraction = DerivedStatement.Extracted(
    DerivedStatement.Known(Conjunction(φ, Conjunction(ψ, χ))),
    AppliedExtraction.fromSimpleExtraction(Seq(
      step(extractRightConjunct, Seq(φ, Conjunction(ψ, χ)), Nil),
      step(extractRightConjunct, Seq(ψ, χ), Nil))))
  val multipleInferenceExtraction = DerivedStatement.Extracted(
    DerivedStatement.Known(Conjunction(φ, Conjunction(ψ, χ))),
    AppliedExtraction.fromSimpleExtraction(Seq(
      step(extractRightConjunct, Seq(φ, Conjunction(ψ, χ)), Nil),
      step(extractLeftConjunct, Seq(ψ, χ), Nil))))
  val conjunctionCombination = step(combineConjunction, Seq(φ, ψ), Nil)
  // Trivial
  val reversal = step(reverseEquality, Nil, Seq(a, c))
  val substitution = step(substitutionOfEquals, Seq(ElementOf(a, $)), Seq(Domain(c), d))
  val transitivity = step(equalityIsTransitive, Nil, Seq(a, b, c))

  "primary inference" should {
    "be absent for a known statement" in {
      known.primaryInference must beNone
    }

    "be the inference of a derivation with a single step" in {
      DerivedStatement.Simplified(known, simplification).primaryInference must beSome(simplification.inference.summary)
      DerivedStatement.Reversed(known, reversal).primaryInference must beSome(reverseEquality.summary)
      DerivedStatement.Asserted(applied(equalityIsReflexive, Seq(a))).primaryInference must beSome(equalityIsReflexive.summary)
      DerivedStatement.Composed(Seq(known, otherKnown), conjunctionCombination).primaryInference must beSome(combineConjunction.summary)
      DerivedStatement.Composed(Seq(known, otherKnown), functionApplication).primaryInference must beSome(functionApplicationIsElementOfRange.summary)
      DerivedStatement.RelationRewritten(known, Some(otherKnown), otherSimplification).primaryInference must beSome(otherSimplification.inference.summary)
      DerivedStatement.Substituted(known, otherKnown, substitution).primaryInference must beSome(substitutionOfEquals.summary)
      DerivedStatement.Transitive(known, otherKnown, transitivity).primaryInference must beSome(equalityIsTransitive.summary)
    }

    "be the shared inference of nested simplifications" in {
      val inner = DerivedStatement.Simplified(known, simplification)
      DerivedStatement.Simplified(inner, simplification).primaryInference must beSome(simplification.inference.summary)
    }

    "be absent for nested simplifications with different inferences" in {
      val inner = DerivedStatement.Simplified(known, simplification)
      DerivedStatement.Simplified(inner, otherSimplification).primaryInference must beNone
    }

    "be the inference of an extraction with a single inference" in {
      singleInferenceExtraction.primaryInference must beSome(extractRightConjunct.summary)
    }

    "be absent for an extraction with multiple inferences" in {
      multipleInferenceExtraction.primaryInference must beNone
    }

    "prefer a substantive inference to an extraction" in {
      DerivedStatement.Simplified(singleInferenceExtraction, simplification).primaryInference must beSome(simplification.inference.summary)
      DerivedStatement.Simplified(multipleInferenceExtraction, simplification).primaryInference must beSome(simplification.inference.summary)
      DerivedStatement.RelationRewritten(singleInferenceExtraction, None, otherSimplification).primaryInference must beSome(otherSimplification.inference.summary)
    }

    "be absent for different substantive inferences" in {
      DerivedStatement.RelationRewritten(DerivedStatement.Simplified(known, simplification), None, otherSimplification).primaryInference must beNone
      DerivedStatement.Composed(Seq(DerivedStatement.Simplified(known, simplification), otherKnown), functionApplication).primaryInference must beNone
    }

    "treat combining a defined statement as a simplification" in {
      DerivedStatement.Composed(Seq(DerivedStatement.Simplified(known, simplification), otherKnown), conjunctionCombination)
        .primaryInference must beSome(simplification.inference.summary)
      DerivedStatement.Composed(Seq(singleInferenceExtraction, otherKnown), conjunctionCombination)
        .primaryInference must beNone
    }

    "pass through a reversal" in {
      DerivedStatement.Reversed(DerivedStatement.Simplified(known, simplification), reversal).primaryInference must beSome(simplification.inference.summary)
      DerivedStatement.Reversed(singleInferenceExtraction, reversal).primaryInference must beSome(extractRightConjunct.summary)
    }

    "not fall back to a reversal when an extraction is ambiguous" in {
      DerivedStatement.Reversed(multipleInferenceExtraction, reversal).primaryInference must beNone
    }

    "pass through a substitution" in {
      DerivedStatement.Substituted(singleInferenceExtraction, known, substitution).primaryInference must beSome(extractRightConjunct.summary)
      DerivedStatement.Substituted(DerivedStatement.Composed(Seq(known, otherKnown), functionApplication), known, substitution)
        .primaryInference must beSome(functionApplicationIsElementOfRange.summary)
    }

    "use the derivation of the equality in a substitution" in {
      DerivedStatement.Substituted(known, DerivedStatement.Simplified(known, simplification), substitution)
        .primaryInference must beSome(simplification.inference.summary)
    }

    "be absent for a substitution using a chain of equalities" in {
      DerivedStatement.Substituted(known, DerivedStatement.Transitive(known, otherKnown, transitivity), substitution)
        .primaryInference must beNone
    }
  }
}
