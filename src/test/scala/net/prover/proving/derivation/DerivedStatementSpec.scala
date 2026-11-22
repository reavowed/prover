package net.prover.proving.derivation

import net.prover.{ContextHelper, StepHelpers}
import net.prover.model.*
import net.prover.model.TestDefinitions.{a, b, *}
import net.prover.model.expressions.{Statement, Term, TermVariable}
import net.prover.model.proof.{Step, SubstitutionContext}
import net.prover.proving.derivation.DerivedStatement.*
import net.prover.proving.extraction.{AppliedExtraction, AppliedInferenceExtraction}
import org.specs2.mutable.Specification

/**
  * The inference that describes a derivation is that of the highest tier of inference it uses, if that tier only uses
  * one inference:
  *   Composition > Relation rewrite > Fact > Simplification > Extraction > Rewrite
  * A logical combination is only described by its own inference if nothing other than rewrites was needed to find its
  * components.
  */
class DerivedStatementSpec extends Specification with StepHelpers with ContextHelper {
  given SubstitutionContext = SubstitutionContext.outsideProof
  given availableEntries: AvailableEntries = defaultAvailableEntries

  val PositiveIntegersDefinition = simpleTermDefinition("ℤ⁺", Nil, Format.default(0))
  val PositiveIntegers = PositiveIntegersDefinition()
  val RationalsDefinition = simpleTermDefinition("ℚ", Nil, Format.default(0))
  val Rationals = RationalsDefinition()
  val RealsDefinition = simpleTermDefinition("ℝ", Nil, Format.default(0))
  val Reals = RealsDefinition()

  // Simplifications
  val firstCoordinates = createInference("First Coordinates of Equal Ordered Pairs Are Equal", Seq(Equals(Pair(a, b), Pair(c, d))), Equals(a, c))
  val firstCoordinate = firstCoordinateOfElementOfCartesianProduct
  val secondCoordinate = secondCoordinateOfElementOfCartesianProduct
  // Relation rewrites
  val positiveIsGreaterThanZero = createInference("Positive Integers Are Greater Than Zero", Seq(ElementOf(a, PositiveIntegers)), TestDefinitions.lessThan(Zero, a))
  val elementOfSubset = createInference("Element of Subset", Seq(Subset(a, b), ElementOf(c, a)), ElementOf(c, b))
  val naturalsAreIntegers = createInference("Natural Numbers Are Integers", Seq(ElementOf(a, Naturals)), ElementOf(a, Integers))
  val lessThanImpliesNotEqual = createInference("Less Than Implies Not Equal", Seq(TestDefinitions.lessThan(a, b)), Negation(Equals(a, b)))
  val equalImpliesLessThanOrEqual = createInference("Equal Implies Less Than or Equal", Seq(Equals(a, b)), Negation(TestDefinitions.lessThan(b, a)))
  // Facts
  val oneIsPositive = createInference("1 Is a Positive Integer", Nil, ElementOf(One, PositiveIntegers))
  val naturalsAreRationals = createInference("ℕ ⊆ ℚ", Nil, Subset(Naturals, Rationals))
  val rationalsAreReals = createInference("ℚ ⊆ ℝ", Nil, Subset(Rationals, Reals))
  // Compositions
  val naturalAdditionClosure = createInference("Natural Addition Closure", Seq(ElementOf(a, Naturals), ElementOf(b, Naturals)), ElementOf(add(a, b), Naturals))
  val integerAdditionClosure = createInference("Integer Addition Closure", Seq(ElementOf(a, Integers), ElementOf(b, Integers)), ElementOf(add(a, b), Integers))
  val positiveAdditionClosure = createInference("Positive Integer Addition Closure", Seq(ElementOf(a, PositiveIntegers), ElementOf(b, PositiveIntegers)), ElementOf(add(a, b), PositiveIntegers))
  val naturalMultiplicationClosure = createInference("Natural Multiplication Closure", Seq(ElementOf(a, Naturals), ElementOf(b, Naturals)), ElementOf(multiply(a, b), Naturals))

  def step(inference: Inference, statements: Seq[Statement], terms: Seq[Term]): Step.AssertionStep = {
    assertion(inference, statements, terms)(summon)
  }
  def termStep(inference: Inference): Step.AssertionStep = step(inference, Nil, inference.variableDefinitions.terms.indices.map(TermVariable(_)))
  def applied(inference: Inference): AppliedInferenceExtraction = AppliedInferenceExtraction(termStep(inference), AppliedExtraction.empty)

  val known = Known(φ)
  def reversed(source: DerivedStatement) = Reversed(source, termStep(reverseEquality))
  def substituted(source: DerivedStatement) = Substituted(source, known, step(substitutionOfEquals, Seq(ElementOf(a, $)), Seq(b, c)))
  def extractedLeftConjunct(source: DerivedStatement) = Extracted(source, AppliedExtraction.fromSimpleExtraction(Seq(step(extractLeftConjunct, Seq(φ, ψ), Nil))))
  def specified(source: DerivedStatement) = Extracted(source, AppliedExtraction.fromSimpleExtraction(Seq(step(specification, Seq(φ($)), Seq(a)))))
  def simplified(source: DerivedStatement, inference: Inference) = Simplified(source, applied(inference))
  def rewritten(source: DerivedStatement, inference: Inference) = RelationRewritten(source, None, applied(inference))
  def rewritten(source: DerivedStatement, supportingPremise: DerivedStatement, inference: Inference) = RelationRewritten(source, Some(supportingPremise), applied(inference))
  def fact(inference: Inference) = Asserted(applied(inference))
  def composed(inference: Inference, components: DerivedStatement*) = Composed(components, termStep(inference))
  def combined(components: DerivedStatement*) = Combined(components, step(combineConjunction, Seq(φ, ψ), Nil))
  val substitution = step(substitutionOfEquals, Seq(ElementOf(a, $)), Seq(b, c))

  def checkPrimaryInference(description: String, actual: => Option[Inference.Summary], expected: Option[Inference]) = {
    description in {
      actual must beEqualTo(expected.map(_.summary))
    }
  }
  def check(description: String, derivation: => DerivedStatement, expected: Option[Inference]) = {
    checkPrimaryInference(description, derivation.primaryInference, expected)
  }

  "primary inference" should {
    "be absent for a known statement" in {
      known.primaryInference must beNone
    }

    // No relation rewrites
    check("be the simplification for a reversed simplification, e.g. c = a from (a, b) = (c, d)", reversed(simplified(known, firstCoordinates)), Some(firstCoordinates))
    checkPrimaryInference("be the simplification when rewriting with a reversed simplification", reversed(simplified(known, firstCoordinates)).primaryInferenceOfRewrite(substitution), Some(firstCoordinates))
    check("be the reversal for a reversed premise, e.g. (c, d) = (a, b) from (a, b) = (c, d)", reversed(known), Some(reverseEquality))
    checkPrimaryInference("be absent when rewriting with a reversed premise", reversed(known).primaryInferenceOfRewrite(substitution), None)
    check("be the extraction for a substitution into an extraction, e.g. x ∈ A from x ∈ domain(f) ∧ φ, A = domain(f)", substituted(extractedLeftConjunct(known)), Some(extractLeftConjunct))
    check("be the composition for a substitution into a composition, e.g. f(a) ∈ A from f is a function, a ∈ domain(f), A = range(f)", substituted(composed(functionApplicationIsElementOfRange, known, known)), Some(functionApplicationIsElementOfRange))
    check("be the composition for a composition using a fact, e.g. 0 + a ∈ ℕ from a ∈ ℕ", composed(naturalAdditionClosure, fact(zeroIsANaturalNumber), known), Some(naturalAdditionClosure))
    check("be absent for nested compositions with different inferences, e.g. (a + b) · c ∈ ℕ from a, b, c ∈ ℕ", composed(naturalMultiplicationClosure, composed(naturalAdditionClosure, known, known), known), None)

    // Relation rewrites with rewrites and extractions
    check("be the relation rewrite for a reversed relation rewrite, e.g. b ≠ a from a < b", reversed(rewritten(known, lessThanImpliesNotEqual)), Some(lessThanImpliesNotEqual))
    check("be the relation rewrite for a relation rewrite of a reversal, e.g. a ≤ b from b = a", rewritten(reversed(known), equalImpliesLessThanOrEqual), Some(equalImpliesLessThanOrEqual))
    check("be the relation rewrite for a relation rewrite of a substitution, e.g. x ∈ ℤ from x ∈ A, A = ℕ", rewritten(substituted(known), naturalsAreIntegers), Some(naturalsAreIntegers))
    check("be the relation rewrite for a relation rewrite of an extraction, e.g. 0 < x from x ∈ ℤ⁺ ∧ φ", rewritten(extractedLeftConjunct(known), positiveIsGreaterThanZero), Some(positiveIsGreaterThanZero))
    check("be the relation rewrite for a relation rewrite with an extracted supporting premise, e.g. x ∈ B from x ∈ A, A ⊆ B ∧ ψ", rewritten(known, extractedLeftConjunct(known), elementOfSubset), Some(elementOfSubset))

    // Relation rewrites with simplifications
    check("be the relation rewrite for a relation rewrite of a simplification, e.g. 0 < a from (a, b) ∈ ℤ⁺ × B", rewritten(simplified(known, firstCoordinate), positiveIsGreaterThanZero), Some(positiveIsGreaterThanZero))
    check("be the relation rewrite for a relation rewrite of a repeated simplification, e.g. 0 < a from ((a, b), c) ∈ (ℤ⁺ × B) × C", rewritten(simplified(simplified(known, firstCoordinate), firstCoordinate), positiveIsGreaterThanZero), Some(positiveIsGreaterThanZero))
    check("be the relation rewrite for a relation rewrite of different simplifications, e.g. 0 < b from ((a, b), c) ∈ (A × ℤ⁺) × C", rewritten(simplified(simplified(known, firstCoordinate), secondCoordinate), positiveIsGreaterThanZero), Some(positiveIsGreaterThanZero))
    check("be the relation rewrite for a repeated relation rewrite of a simplification, e.g. x ∈ C from (x, y) ∈ A × B, A ⊆ M, M ⊆ C", rewritten(rewritten(simplified(known, firstCoordinate), known, elementOfSubset), known, elementOfSubset), Some(elementOfSubset))
    check("be absent for different relation rewrites of a simplification, e.g. 0 ≠ a from (a, b) ∈ ℤ⁺ × B", rewritten(rewritten(simplified(known, firstCoordinate), positiveIsGreaterThanZero), lessThanImpliesNotEqual), None)

    // Relation rewrites together
    check("be the relation rewrite for a repeated relation rewrite, e.g. x ∈ C from x ∈ A, A ⊆ B, B ⊆ C", rewritten(rewritten(known, known, elementOfSubset), known, elementOfSubset), Some(elementOfSubset))
    check("be absent for different relation rewrites, e.g. x ∈ ℤ from x ∈ A, A ⊆ ℕ", rewritten(rewritten(known, known, elementOfSubset), naturalsAreIntegers), None)

    // Relation rewrites with facts
    check("be the relation rewrite for a relation rewrite of a fact, e.g. 0 < 1 from fact 1 ∈ ℤ⁺", rewritten(fact(oneIsPositive), positiveIsGreaterThanZero), Some(positiveIsGreaterThanZero))
    check("be the relation rewrite for a relation rewrite with a fact as its supporting premise, e.g. x ∈ ℝ from x ∈ ℚ, fact ℚ ⊆ ℝ", rewritten(known, fact(rationalsAreReals), elementOfSubset), Some(elementOfSubset))
    check("be the relation rewrite for a relation rewrite of a fact to a larger set, e.g. 0 ∈ ℤ from fact 0 ∈ ℕ", rewritten(fact(zeroIsANaturalNumber), naturalsAreIntegers), Some(naturalsAreIntegers))
    check("be the relation rewrite for a repeated relation rewrite with different facts as supporting premises, e.g. x ∈ ℝ from x ∈ ℕ, facts ℕ ⊆ ℚ, ℚ ⊆ ℝ", rewritten(rewritten(known, fact(naturalsAreRationals), elementOfSubset), fact(rationalsAreReals), elementOfSubset), Some(elementOfSubset))
    check("be absent for different relation rewrites of a fact, e.g. 0 ≠ 1 from fact 1 ∈ ℤ⁺", rewritten(rewritten(fact(oneIsPositive), positiveIsGreaterThanZero), lessThanImpliesNotEqual), None)

    // Relation rewrites with compositions
    check("be the composition for a composition of a relation rewrite, e.g. a + b ∈ ℤ from a ∈ ℕ, b ∈ ℤ", composed(integerAdditionClosure, rewritten(known, naturalsAreIntegers), known), Some(integerAdditionClosure))
    check("be the composition for a composition of repeated relation rewrites, e.g. a + b ∈ ℤ from a, b ∈ ℕ", composed(integerAdditionClosure, rewritten(known, naturalsAreIntegers), rewritten(known, naturalsAreIntegers)), Some(integerAdditionClosure))
    check("be the composition for a relation rewrite of a composition, e.g. 0 < a + b from a, b ∈ ℤ⁺", rewritten(composed(positiveAdditionClosure, known, known), positiveIsGreaterThanZero), Some(positiveAdditionClosure))
    check("be the composition for a relation rewrite of a composition using a fact, e.g. 0 < 1 + a from a ∈ ℤ⁺, fact 1 ∈ ℤ⁺", rewritten(composed(positiveAdditionClosure, fact(oneIsPositive), known), positiveIsGreaterThanZero), Some(positiveAdditionClosure))
    check("be the composition for different relation rewrites of a composition, e.g. 0 ≠ a + b from a, b ∈ ℤ⁺", rewritten(rewritten(composed(positiveAdditionClosure, known, known), positiveIsGreaterThanZero), lessThanImpliesNotEqual), Some(positiveAdditionClosure))

    // Logical combinations
    check("be the combination for a combination of known statements, e.g. φ ∧ ψ from φ, ψ", combined(known, known), Some(combineConjunction))
    check("be absent for a combination using a fact, e.g. 0 ∈ ℕ ∧ φ from φ", combined(fact(zeroIsANaturalNumber), known), None)
    check("be absent for a combination using a simplification, e.g. a = c ∧ φ from (a, b) = (c, d), φ", combined(simplified(known, firstCoordinates), known), None)
    check("be absent for a combination using an extraction, e.g. φ(a) ∧ χ from ∀x φ(x), χ", combined(specified(known), known), None)
    check("be absent for a combination using a relation rewrite, e.g. 0 < x ∧ φ from x ∈ ℤ⁺, φ", combined(rewritten(known, positiveIsGreaterThanZero), known), None)
    check("be absent for a combination using a relation rewrite of a simplification, e.g. 0 < a ∧ φ from (a, b) ∈ ℤ⁺ × B, φ", combined(rewritten(simplified(known, firstCoordinate), positiveIsGreaterThanZero), known), None)
    check("be absent for a combination using a relation rewrite of a fact, e.g. 0 < 1 ∧ φ from fact 1 ∈ ℤ⁺, φ", combined(rewritten(fact(oneIsPositive), positiveIsGreaterThanZero), known), None)
    check("be absent for a combination using a relation rewrite with a fact as its supporting premise, e.g. x ∈ ℝ ∧ φ from x ∈ ℚ, fact ℚ ⊆ ℝ, φ", combined(rewritten(known, fact(rationalsAreReals), elementOfSubset), known), None)
    check("be absent for a combination using different simplifications, e.g. a ∈ A ∧ b ∈ B from (a, b) ∈ A × B", combined(simplified(known, firstCoordinate), simplified(known, secondCoordinate)), None)
    check("be absent for a combination using a composition, e.g. 0 + a ∈ ℕ ∧ φ from a ∈ ℕ, φ", combined(composed(naturalAdditionClosure, fact(zeroIsANaturalNumber), known), known), None)
    check("be the composition for a composition of a combination, as from a theorem whose premise is a conjunction", composed(naturalAdditionClosure, combined(fact(zeroIsANaturalNumber), known)), Some(naturalAdditionClosure))
    check("be the combination for a combination using a reversal, e.g. b = a ∧ φ from a = b, φ", combined(reversed(known), known), Some(combineConjunction))
    check("be the combination for nested combinations, e.g. φ ∧ (ψ ∧ χ) from φ, ψ, χ", combined(known, combined(known, known)), Some(combineConjunction))

    // Extractions
    check("be the extraction for an extraction with a single inference", extractedLeftConjunct(known), Some(extractLeftConjunct))
    check("be absent for an extraction with multiple inferences", Extracted(known, AppliedExtraction.fromSimpleExtraction(Seq(step(specification, Seq(φ($)), Seq(a)), step(extractLeftConjunct, Seq(φ, ψ), Nil)))), None)
    check("be the extraction for an extraction ending in a reversal", Extracted(known, AppliedExtraction(Seq(step(extractLeftConjunct, Seq(φ, ψ), Nil), termStep(reverseEquality)), Nil, true)), Some(extractLeftConjunct))

    // Derivations saved before derivations were structured
    check("be the single inference of an unstructured derivation", Unstructured(φ, Seq(termStep(reverseEquality), termStep(reverseEquality))), Some(reverseEquality))
    check("be absent for an unstructured derivation with multiple inferences", Unstructured(φ, Seq(applied(firstCoordinates), termStep(reverseEquality))), None)
  }
}
