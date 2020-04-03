package net.prover.controllers

import net.prover.controllers.ExtractionHelper.ExtractionApplication
import net.prover.controllers.models.{StepDefinition, Unwrapper}
import net.prover.model.ExpressionParsingContext.TermVariableValidator
import net.prover.model.{ExpressionParsingContext, Substitutions}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{PremiseFinder, ProofHelper, Step, StepProvingContext}
import net.prover.model.proof.SubstatementExtractor.VariableTracker

import scala.util.{Success, Try}

trait StepCreation extends BookModification {
  def createAssertionStepForInference(
    inferenceId: String,
    getConclusionOption: (ExpressionParsingContext, Substitutions) => Try[Option[Statement]],
    definition: StepDefinition,
    unwrappers: Seq[Unwrapper])(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Statement, Step, Seq[Step.Target])] = {
    for {
      inference <- findInference(inferenceId)
      wrappedStepProvingContext = StepProvingContext.updateStepContext(unwrappers.enhanceContext)
      substitutions <- definition.substitutions.parse()(ExpressionParsingContext.atStep(wrappedStepProvingContext))
      extractionInferences <- definition.extractionInferenceIds.map(findInference).traverseTry
      epc = ExpressionParsingContext(implicitly, TermVariableValidator.LimitedList(VariableTracker.fromInference(inference).baseVariableNames ++ definition.additionalVariableNames.toSeq.flatten), Nil)
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
      (mainAssertion, mainPremises, mainTargets) <- ProofHelper.getAssertionWithPremises(inferenceToApply, substitutions)(wrappedStepProvingContext).orBadRequest("Could not apply substitutions to inference")
      ExtractionApplication(result, mainPremise, extractionSteps, extractionPremises, extractionTargets) <-
        ExtractionHelper.applyExtractions(mainAssertion.statement, extractionInferences, inference, substitutions, newTargetStatementsForExtractionOption, conclusionOption, PremiseFinder.findPremiseStepsOrTargets)(wrappedStepProvingContext)
      mainAssertionWithCorrectConclusion = mainAssertion.copy(statement = mainPremise)
      extractionStep = Step.Elided.ifNecessary(mainAssertionWithCorrectConclusion +: extractionSteps, inference).get
      assertionWithExtractionStep = Step.Elided.ifNecessary(mainPremises ++ extractionPremises :+ extractionStep, inference).get
      (wrappedResult, wrappedStep, wrappedTargets) = if (unwrappers.nonEmpty)
        unwrappers
          .addNecessaryExtractions(result, assertionWithExtractionStep, mainTargets ++ extractionTargets)
          .map2(Step.Elided.forInference(inference)(_))
      else
        (result, assertionWithExtractionStep, mainTargets ++ extractionTargets)
    } yield (wrappedResult, wrappedStep, wrappedTargets)
  }
}
