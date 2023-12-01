package net.prover.proving.extraction

import org.specs2.mutable.Specification
import net.prover.model.TestDefinitions._

class ExtractionCalculatorSpec extends Specification {
  "extraction calculator" should {
    "not use deconstruction inference in extraction of a construction inference" in {
      implicit val availableEntries = defaultAvailableEntries
      val extractions = ExtractionCalculator.getInferenceExtractions(Commutative.statementDefinition.constructionInference.get)
      extractions.find(e => e.extractionDefinition.extractionInferences.contains(Commutative.statementDefinition.deconstructionInference.get)) must beEmpty
    }
  }
}
