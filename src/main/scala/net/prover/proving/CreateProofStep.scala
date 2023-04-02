package net.prover.proving

import net.prover.controllers.models.StepDefinition
import net.prover.controllers.{AnyWithResponseExceptionOps, ExtractionHelper, OptionWithResponseExceptionOps}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepContext, SubstatementExtractor}
import net.prover.model.unwrapping.Unwrapper
import net.prover.model.{ExpressionParsingContext, Substitutions}

import scala.util.Try

object CreateProofStep {
  def apply(
    definition: StepDefinition,
    getConclusionOption: (ExpressionParsingContext, Substitutions) => Try[Option[Statement]],
    unwrappers: Seq[Unwrapper])(
    implicit stepContext: StepContext
  ): Try[(Statement, Step, Seq[Step.Target])] = {
    def withInference(inferenceId: String) = CreateAssertionStep(inferenceId, getConclusionOption, definition, unwrappers)

    def withPremise(serializedPremiseStatement: String) = {
      for {
        premiseStatement <- Statement.parser.parseFromString(serializedPremiseStatement, "premise").recoverWithBadRequest
        premise <- stepContext.findPremise(premiseStatement).orBadRequest(s"Could not find premise $premiseStatement")
        extractionInferences <- definition.extractionInferenceIds.map(FindInference(_)).traverseTry
        extraction <- SubstatementExtractor.getPremiseExtractions(premiseStatement).find(_.extractionInferences == extractionInferences).orBadRequest("Could not find extraction with given inferences")
        substitutions <- definition.substitutions.parse(extraction.variableDefinitions)
        epc = implicitly[ExpressionParsingContext].addSimpleTermVariables(extraction.additionalVariableNames)
        conclusionOption <- getConclusionOption(epc, substitutions)
        newTargetStatementsOption <- definition.parseIntendedPremiseStatements(epc)
        substitutedNewTargetStatementsOption <- newTargetStatementsOption.map(_.map(_.applySubstitutions(substitutions)).traverseOption.orBadRequest("Could not apply substitutions to intended new targets")).swap
        (result, stepOption, extractionTargets) <- ExtractionHelper.getPremiseExtractionWithPremises(premise, extractionInferences, substitutions, substitutedNewTargetStatementsOption, conclusionOption)
        step <- stepOption.orBadRequest("At least one step must be present")
      } yield (result, step, extractionTargets)
    }

    definition.getFromInferenceOrPremise(withInference, withPremise)
  }

}
