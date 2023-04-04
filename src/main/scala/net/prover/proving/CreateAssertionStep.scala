package net.prover.proving

import net.prover.controllers.models.StepDefinition
import net.prover.controllers.{BooleanWithResponseExceptionOps, OptionWithResponseExceptionOps}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepContext}
import net.prover.model.unwrapping.Unwrapper
import net.prover.model.{ExpressionParsingContext, Substitutions}
import net.prover.proving.extraction.{ExtractionHelper, SubstatementExtractor}

import scala.util.{Success, Try}

object CreateAssertionStep {
  def apply(
    inferenceId: String,
    getConclusionOption: (ExpressionParsingContext, Substitutions) => Try[Option[Statement]],
    definition: StepDefinition,
    unwrappers: Seq[Unwrapper])(
    implicit stepContext: StepContext
  ): Try[(Statement, Step, Seq[Step.Target])] = {
    for {
      inference <- FindInference(inferenceId)
      extractionInferences <- definition.extractionInferenceIds.map(FindInference(_)).traverseTry
      extraction <- SubstatementExtractor.getInferenceExtractions(inference).find(_.extractionInferences == extractionInferences).orBadRequest("Could not find extraction with given inferences")
      wrappedStepContext = unwrappers.enhanceStepContext(stepContext)
      substitutions <- definition.substitutions.parse(extraction.variableDefinitions)(ExpressionParsingContext.atStep(wrappedStepContext))
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
      (derivationStep, targets) <- ExtractionHelper.getInferenceExtractionWithPremises(
        inferenceToApply,
        extractionInferences,
        substitutions,
        unwrappers,
        newTargetStatementsForExtractionOption,
        conclusionOption)(
        stepContext)
    } yield (derivationStep.statement, derivationStep, targets)
  }
}
