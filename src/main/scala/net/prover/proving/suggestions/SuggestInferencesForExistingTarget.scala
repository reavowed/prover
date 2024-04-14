package net.prover.proving.suggestions

import net.prover.controllers.BookService
import net.prover.controllers.models._
import net.prover.model.Inference
import net.prover.model.proof.Step
import net.prover.model.unwrapping.{GeneralizationUnwrapper, UnwrappedStatement}

import scala.util.Try

object SuggestInferencesForExistingTarget {
  def apply(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepReference: PathData,
    searchText: String)(
    implicit bookService: BookService
  ): Try[Seq[PossibleInferenceWithTargets]] = {
    bookService.findStep[Step.TargetStep](bookKey, chapterKey, theoremKey, proofIndex, stepReference).map(implicit stepWithContext => {
      val possibleUnwrappedTargetStatements = UnwrappedStatement.getUnwrappedStatements(stepWithContext.step.statement)

      def findPossibleInference(inference: Inference): Option[PossibleInferenceWithTargets] = {
        val possibleTargets = for {
          possibleUnwrappedTargetStatement <- possibleUnwrappedTargetStatements
          possibleConclusions = stepWithContext.provingContext.inferenceExtractionsByInferenceId(inference.id)
            .filter(_.conclusion.calculateSubstitutions(possibleUnwrappedTargetStatement.statement).nonEmpty)
            .map(e => PossibleConclusionWithoutPremises(e.conclusion, e.extractionDefinition.serialized, e.additionalVariableNames))
          if possibleConclusions.nonEmpty
        } yield PossibleTarget(
          possibleUnwrappedTargetStatement.statement,
          possibleUnwrappedTargetStatement.definitionSymbols,
          possibleUnwrappedTargetStatement.unwrappers.ofType[GeneralizationUnwrapper].map(_.variableName).map(Seq(_)),
          possibleConclusions)

        if (possibleTargets.nonEmpty)
          Some(PossibleInferenceWithTargets(inference.summary, possibleTargets))
        else
          None
      }

      def getConclusionComplexity(possibleConclusion: PossibleConclusion): Int = possibleConclusion.conclusion.structuralComplexity

      SuggestInferences(
        searchText,
        findPossibleInference,
        getConclusionComplexity)
    })
  }
}
