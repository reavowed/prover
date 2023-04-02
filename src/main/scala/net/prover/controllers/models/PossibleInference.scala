package net.prover.controllers.models

import net.prover.model.expressions.{Expression, Statement}
import net.prover.model.proof.StepContext
import net.prover.model.{Inference, Substitutions, VariableDefinitions}
import net.prover.proving.extraction.SubstatementExtractor.Extraction

sealed trait PossibleInference {
  def inference: Inference.Summary
  def canEqual(other: Any): Boolean = other.isInstanceOf[PossibleInference]
  override def equals(other: Any): Boolean = other match {
    case that: PossibleInference =>
      (that canEqual this) &&
        inference == that.inference
    case _ => false
  }
  override def hashCode(): Int = {
    inference.hashCode
  }
}

case class PossibleInferenceWithTargets(inference: Inference.Summary, possibleTargets: Seq[PossibleTarget]) extends PossibleInference
case class PossibleInferenceWithConclusions(inference: Inference.Summary, possibleConclusions: Seq[PossibleConclusionWithPremises]) extends PossibleInference

case class PossibleTarget(
  target: Expression,
  wrappingDefinitions: Seq[String],
  additionalBoundVariables: Seq[Seq[String]],
  possibleConclusions: Seq[PossibleConclusion])

sealed trait PossibleConclusion {
  def conclusion: Statement
  def extractionInferenceIds: Seq[String]
  def additionalVariableNames: Seq[String]
}

case class PossibleConclusionWithoutPremises(
  conclusion: Statement,
  extractionInferenceIds: Seq[String],
  additionalVariableNames: Seq[String]
) extends PossibleConclusion

case class PossibleConclusionWithPremises(
  conclusion: Statement,
  possiblePremises: Seq[PossiblePremise],
  substitutions: Option[SuggestedSubstitutions],
  variableDefinitions: VariableDefinitions,
  extractionInferenceIds: Seq[String],
  additionalVariableNames: Seq[String]
) extends PossibleConclusion

object PossibleConclusionWithPremises {
  def fromExtractionWithTarget(extraction: Extraction, target: Statement)(implicit stepContext: StepContext): Option[PossibleConclusionWithPremises] = {
    fromExtractionWithSubstitutions(extraction, _.calculateSubstitutions(target))
  }
  def fromExtractionWithSubstitutions(extraction: Extraction, getSubstitutions: Statement => Option[Substitutions.Possible])(implicit stepContext: StepContext): Option[PossibleConclusionWithPremises] = {
    getSubstitutions(extraction.conclusion).map(s => fromExtraction(extraction, Some(s)))
  }
  def fromExtraction(extraction: Extraction, substitutions: Option[Substitutions.Possible])(implicit stepContext: StepContext): PossibleConclusionWithPremises = {
    PossibleConclusionWithPremises(
      extraction.conclusion,
      PossiblePremise.fromAvailablePremises(extraction.premises, substitutions, extraction.variableDefinitions),
      substitutions.map(SuggestedSubstitutions(extraction.variableDefinitions, _)),
      extraction.variableDefinitions,
      extraction.extractionInferences.map(_.id),
      extraction.additionalVariableNames)
  }
}

case class PossiblePremise(
  premise: Statement,
  possibleMatches: Seq[PossiblePremiseMatch])

object PossiblePremise {
  def fromAvailablePremises(
    premises: Seq[Statement],
    substitutions: Option[Substitutions.Possible],
    variableDefinitions: VariableDefinitions)(
    implicit stepContext: StepContext
  ): Seq[PossiblePremise] = {
    premises.map { premise =>
      val matches = (stepContext.allPremises.map(_.statement) ++ stepContext.provingContext.facts.map(_.statement)).mapCollect { availablePremise =>
        premise.calculateSubstitutions(availablePremise, substitutions.getOrElse(Substitutions.Possible.empty))
          .map(s => PossiblePremiseMatch(availablePremise, SuggestedSubstitutions(variableDefinitions, s)))
      }
      PossiblePremise(premise, matches)
    }
  }
}

case class PossiblePremiseMatch(matchingPremise: Statement, substitutions: SuggestedSubstitutions)
