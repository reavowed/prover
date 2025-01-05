package net.prover.proving.extraction

import net.prover.StepBuilderHelper
import net.prover.model.{Substitutions, TermVariablePlaceholder}
import org.specs2.mutable.Specification
import net.prover.model.TestDefinitions._
import net.prover.model.expressions.DefinedStatement
import net.prover.model.proof.Step
import net.prover.model.proof.Step.{AssertionStep, InferenceExtractionStep}

class ExtractionApplierSpec extends Specification with StepBuilderHelper {

  implicit val availableEntries = defaultAvailableEntries

  "extraction applier" should {
    "match bound variables to an intended conclusion" in {
      val f = TermVariablePlaceholder("f", 0)
      val x = TermVariablePlaceholder("x", 0)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(f -> 0, x-> 0))
      implicit val outerStepContext = createOuterStepContext(Nil)

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
        .extractionSteps.last.statement.asInstanceOf[DefinedStatement]
        .boundVariableNames mustEqual Seq("y")
      extractionStep.get._1.asInstanceOf[InferenceExtractionStep]
        .extractionSteps.last.asInstanceOf[AssertionStep]
        .substitutions.statements.last.asInstanceOf[DefinedStatement]
        .boundVariableNames mustEqual Seq("y")
      extractionStep.get._1.asInstanceOf[Step.InferenceExtractionStep]
        .extractionSteps.init.last.statement.asInstanceOf[DefinedStatement]
        .components.last.asInstanceOf[DefinedStatement]
        .boundVariableNames mustEqual Seq("y")
      extractionStep.get._1.asInstanceOf[Step.InferenceExtractionStep]
        .extractionSteps.init.last.asInstanceOf[AssertionStep]
        .substitutions.statements.last.asInstanceOf[DefinedStatement]
        .components.last.asInstanceOf[DefinedStatement]
        .boundVariableNames mustEqual Seq("y")
    }
  }
}
