package net.prover.proving.extraction

import net.prover.model.TestDefinitions.{*, given}
import net.prover.model.{AvailableEntries, VariableDefinitions}
import org.specs2.mutable.Specification

class ExtractionCalculatorSpec extends Specification {
  "extraction calculator" should {
    "find an extraction with a left rewrite" in {
      val disjoinedConjunctEquivalence = createInference(
        "Disjoined Conjunct Equivalence",
        Seq(φ, Negation(χ)),
        Equivalence(Disjunction(Conjunction(φ, ψ), Conjunction(χ, ω)), φ))
      val orIsSymmetric = createInference(
        "Or Is Symmetric",
        Nil,
        Equivalence(Disjunction(φ, ψ), Disjunction(ψ, φ)))
      given availableEntries: AvailableEntries = defaultAvailableEntries.addEntry(disjoinedConjunctEquivalence).addEntry(orIsSymmetric)

      val extractions = ExtractionCalculator.getInferenceExtractions(disjoinedConjunctEquivalence)

      extractions must contain (((e: InferenceExtraction) => e.conclusion) ^^ beEqualTo(Equivalence(Disjunction(Conjunction(χ, ω), Conjunction(φ, ψ)), φ)))
    }
    "find an extraction with a right rewrite" in {
      val disjoinedConjunctEquivalence = createInference(
        "Disjoined Conjunct Equivalence",
        Seq(φ, Negation(χ)),
        Equivalence(Disjunction(Conjunction(φ, ψ), Conjunction(χ, ω)), φ))
      val orIsSymmetric = createInference(
        "Or Is Symmetric",
        Nil,
        Equivalence(Disjunction(φ, ψ), Disjunction(ψ, φ)))
      given availableEntries: AvailableEntries = defaultAvailableEntries.addEntry(disjoinedConjunctEquivalence).addEntry(orIsSymmetric)

      val extractions = ExtractionCalculator.getInferenceExtractions(disjoinedConjunctEquivalence)

      extractions must contain (((e: InferenceExtraction) => e.conclusion) ^^ beEqualTo(Equivalence(φ, Disjunction(Conjunction(χ, ω), Conjunction(φ, ψ)))))
    }

    "not use deconstruction inference in extraction of a construction inference" in {
      given availableEntries: AvailableEntries = defaultAvailableEntries
      val extractions = ExtractionCalculator.getInferenceExtractions(Commutative.statementDefinition.constructionInference.get)
      extractions.find(e => e.extractionDefinition.extractionInferences.contains(Commutative.statementDefinition.deconstructionInference.get)) must beEmpty
    }
  }
}
