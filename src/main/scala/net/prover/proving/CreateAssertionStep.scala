package net.prover.proving

import net.prover.controllers.{BooleanWithResponseExceptionOps, ExtractionHelper, OptionWithResponseExceptionOps}
import net.prover.controllers.models.StepDefinition
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepProvingContext, SubstatementExtractor}
import net.prover.model.unwrapping.Unwrapper
import net.prover.model.{ExpressionParsingContext, Substitutions}

import scala.util.{Success, Try}

object CreateAssertionStep {
  def apply(
    inferenceId: String,
    getConclusionOption: (ExpressionParsingContext, Substitutions) => Try[Option[Statement]],
    definition: StepDefinition,
    unwrappers: Seq[Unwrapper])(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Statement, Step, Seq[Step.Target])] = {
    for {
      inference <- FindInference(inferenceId)
      extractionInferences <- definition.extractionInferenceIds.map(FindInference(_)).traverseTry
      extraction <- SubstatementExtractor.getInferenceExtractions(inference).find(_.extractionInferences == extractionInferences).orBadRequest("Could not find extraction with given inferences")
      wrappedStepProvingContext = StepProvingContext.updateStepContext(unwrappers.enhanceStepContext)
      substitutions <- definition.substitutions.parse(extraction.variableDefinitions)(ExpressionParsingContext.atStep(wrappedStepProvingContext))
      epc = ExpressionParsingContext.forInference(inference).addSimpleTermVariables(definition.additionalVariableNames.toSeq.flatten)
      conclusionOption <- getConclusionOption(epc, substitutions)
      newTargetStatementsOption <- definition.parseIntendedPremiseStatements(epc)
      (inferenceToApply, newTargetStatementsForExtractionOption) <- newTargetStatementsOption match {
        case Some(newTargetStatements) =>
          for {
            (targetStatementsForInference, targetStatementsForExtraction) <- newTargetStatements.takeAndRemainingIfValid(inference.premises.length).orBadRequest("Not enough target statements provided")
            substitutedTargetStatementsForExtraction <- targetStatementsForExtraction.map(_.applySubstitutions(substitutions)).traverseOption.orBadRequest("Could not apply substitutions to extraction premises")
            _ <- (targetStatementsForInference == inference.premises).orBadRequest("Target statements did not match inference premise")
          } yield (inference.copy(premises = targetStatementsForInference), Some(substitutedTargetStatementsForExtraction))
        case None =>
          Success((inference, None))
      }
      (derivationStep, targets) <- ExtractionHelper.getInferenceExtractionWithPremises(inferenceToApply, extractionInferences, substitutions, newTargetStatementsForExtractionOption, conclusionOption)(wrappedStepProvingContext)
      (wrappedResult, wrappedStep, wrappedTargets) = if (unwrappers.nonEmpty)
        unwrappers
          .addNecessaryExtractions(derivationStep.statement, derivationStep.step, targets)
          .map2(Step.Elided.forInference(inference)(_))
      else
        (derivationStep.statement, derivationStep.step, targets)
    } yield (wrappedResult, wrappedStep, wrappedTargets)
  }
}
