package net.prover.proving

import net.prover.controllers.models.StepDefinition
import net.prover.controllers.{AnyWithResponseExceptionOps, OptionWithResponseExceptionOps}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepProvingContext}
import net.prover.model.unwrapping.Unwrapper
import net.prover.model.{ExpressionParsingContext, Substitutions}
import net.prover.proving.extraction.{ExtractionApplier, ExtractionCalculator}

import scala.util.Try

object CreateProofStep {
  def apply(
    definition: StepDefinition,
    getConclusionOption: (ExpressionParsingContext, Substitutions) => Try[Option[Statement]],
    unwrappers: Seq[Unwrapper])(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Statement, Step, Seq[Step.Target])] = {
    def withInference(inferenceId: String) = CreateAssertionStep(inferenceId, getConclusionOption, definition, unwrappers)

    def withPremise(serializedPremiseStatement: String) = {
      for {
        premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise").recoverWithBadRequest
        premise <- stepProvingContext.findPremise(premiseStatement).orBadRequest(s"Could not find premise $premiseStatement")
        extraction <- ExtractionCalculator.getPremiseExtractions(premiseStatement).find(_.extractionDefinition.matches(definition.extractionDefinition)).orBadRequest("Could not find extraction with given inferences")
        substitutions <- definition.substitutions.parse(extraction.variableDefinitions)
        epc = implicitly[ExpressionParsingContext].addSimpleTermVariables(extraction.additionalVariableNames)
        conclusionOption <- getConclusionOption(epc, substitutions)
        newTargetStatementsOption <- definition.parseIntendedPremiseStatements(epc)
        substitutedNewTargetStatementsOption <- newTargetStatementsOption.map(_.map(_.applySubstitutions(substitutions)).traverseOption.orBadRequest("Could not apply substitutions to intended new targets")).swap
        (result, stepOption, extractionTargets) <- ExtractionApplier.getPremiseExtractionStepWithPremises(premise, extraction, substitutions, substitutedNewTargetStatementsOption, conclusionOption)
        step <- stepOption.orBadRequest("At least one step must be present")
      } yield (result, step, extractionTargets)
    }
    definition.getFromInferenceOrPremise(withInference, withPremise)
  }
}
