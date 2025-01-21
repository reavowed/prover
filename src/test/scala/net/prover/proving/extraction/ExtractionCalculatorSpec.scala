package net.prover.proving.extraction

import org.specs2.mutable.Specification
import net.prover.model.TestDefinitions._

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
      implicit val availableEntries = defaultAvailableEntries.addEntry(disjoinedConjunctEquivalence).addEntry(orIsSymmetric)

      val extractions = ExtractionCalculator.getInferenceExtractions(disjoinedConjunctEquivalence)

      extractions must contain (beEqualTo(Equivalence(Disjunction(Conjunction(χ, ω), Conjunction(φ, ψ)), φ)) ^^ { e: InferenceExtraction => e.conclusion })
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
      implicit val availableEntries = defaultAvailableEntries.addEntry(disjoinedConjunctEquivalence).addEntry(orIsSymmetric)

      val extractions = ExtractionCalculator.getInferenceExtractions(disjoinedConjunctEquivalence)

      extractions must contain (beEqualTo(Equivalence(φ, Disjunction(Conjunction(χ, ω), Conjunction(φ, ψ)))) ^^ { e: InferenceExtraction => e.conclusion })
    }

    "not use deconstruction inference in extraction of a construction inference" in {
      implicit val availableEntries = defaultAvailableEntries
      val extractions = ExtractionCalculator.getInferenceExtractions(Commutative.statementDefinition.constructionInference.get)
      extractions.find(e => e.extractionDefinition.extractionInferences.contains(Commutative.statementDefinition.deconstructionInference.get)) must beEmpty
    }
  }
}
