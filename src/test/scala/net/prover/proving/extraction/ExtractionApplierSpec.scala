package net.prover.proving.extraction

import net.prover.StepBuilderHelper
import net.prover.model.TestDefinitions.*
import net.prover.model.expressions.DefinedStatement
import net.prover.model.proof.{Step, StepContext}
import net.prover.model.proof.Step.{AssertionStep, InferenceExtractionStep}
import net.prover.model.{AvailableEntries, Substitutions, TermVariablePlaceholder, VariableDefinitions}
import org.specs2.mutable.Specification

class ExtractionApplierSpec extends Specification with StepBuilderHelper {

  given availableEntries: AvailableEntries = defaultAvailableEntries

  "extraction applier" should {
    "match bound variables to an intended conclusion" in {
      val f = TermVariablePlaceholder("f", 0)
      val x = TermVariablePlaceholder("x", 0)
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Nil, Seq(f -> 0, x-> 0))
      given outerStepContext: StepContext = createOuterStepContext(Nil)

      val extraction = ExtractionCalculator.getInferenceExtractions(Function.deconstructionInference)
        .find(_.extractionDetails.extractionDefinition.extractionInferences == Seq(extractRightConjunct, specification, modusPonens))
        .get
      val extractionStep = ExtractionApplier.getInferenceExtractionStepWithPremises(
        extraction,
        Substitutions(Nil, Seq(f, x)),
        Nil,
        None,
        Some(ExistsUnique("y")(ElementOf(Pair(x, $), f))))

      extractionStep.get._1.asInstanceOf[InferenceExtractionStep]
        .inferenceExtraction.extraction.extractionSteps.last.statement.asInstanceOf[DefinedStatement]
        .boundVariableNames must beEqualTo(Seq("y"))
      extractionStep.get._1.asInstanceOf[InferenceExtractionStep]
        .inferenceExtraction.extraction.extractionSteps.last.toProofStep.asInstanceOf[AssertionStep]
        .substitutions.statements.last.asInstanceOf[DefinedStatement]
        .boundVariableNames must beEqualTo(Seq("y"))
      extractionStep.get._1.asInstanceOf[Step.InferenceExtractionStep]
        .inferenceExtraction.extraction.extractionSteps.init.last.statement.asInstanceOf[DefinedStatement]
        .components.last.asInstanceOf[DefinedStatement]
        .boundVariableNames must beEqualTo(Seq("y"))
      extractionStep.get._1.asInstanceOf[Step.InferenceExtractionStep]
        .inferenceExtraction.extraction.extractionSteps.init.last.toProofStep.asInstanceOf[AssertionStep]
        .substitutions.statements.last.asInstanceOf[DefinedStatement]
        .components.last.asInstanceOf[DefinedStatement]
        .boundVariableNames must beEqualTo(Seq("y"))
    }
  }
}
