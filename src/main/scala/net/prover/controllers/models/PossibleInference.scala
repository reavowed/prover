package net.prover.controllers.models

import net.prover.model.{Inference, Substitutions}
import net.prover.model.expressions.{Expression, Statement}
import net.prover.model.proof.StepProvingContext
import net.prover.model.proof.SubstatementExtractor.ExtractionOption

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
  requiredSubstitutions: Substitutions.Required,
  extractionInferenceIds: Seq[String],
  additionalVariableNames: Seq[String]
) extends PossibleConclusion

object PossibleConclusionWithPremises {
  def fromExtractionOptionWithTarget(extractionOption: ExtractionOption, target: Statement)(implicit stepProvingContext: StepProvingContext): Option[PossibleConclusionWithPremises] = {
    fromExtractionOptionWithSubstitutions(extractionOption, _.calculateSubstitutions(target))
  }
  def fromExtractionOptionWithSubstitutions(extractionOption: ExtractionOption, getSubstitutions: Statement => Option[Substitutions.Possible])(implicit stepProvingContext: StepProvingContext): Option[PossibleConclusionWithPremises] = {
    getSubstitutions(extractionOption.conclusion).map(s => fromExtractionOption(extractionOption, Some(s)))
  }
  def fromExtractionOption(extractionOption: ExtractionOption, substitutions: Option[Substitutions.Possible])(implicit stepProvingContext: StepProvingContext): PossibleConclusionWithPremises = {
    PossibleConclusionWithPremises(
      extractionOption.conclusion,
      PossiblePremise.fromAvailablePremises(extractionOption.premises, substitutions),
      substitutions.map(SuggestedSubstitutions(_)),
      extractionOption.requiredSubstitutions,
      extractionOption.extractionInferences.map(_.id),
      extractionOption.additionalVariableNames)
  }
}

case class PossiblePremise(
  premise: Statement,
  possibleMatches: Seq[PossiblePremiseMatch])

object PossiblePremise {
  def fromAvailablePremises(
    premises: Seq[Statement],
    substitutions: Option[Substitutions.Possible])(
    implicit stepProvingContext: StepProvingContext
  ): Seq[PossiblePremise] = {
    premises.map { premise =>
      val matches = (stepProvingContext.allPremises.map(_.statement) ++ stepProvingContext.provingContext.facts.map(_.statement)).mapCollect { availablePremise =>
        premise.calculateSubstitutions(availablePremise, substitutions.getOrElse(Substitutions.Possible.empty))
          .map(s => PossiblePremiseMatch(availablePremise, SuggestedSubstitutions(s)))
      }
      PossiblePremise(premise, matches)
    }
  }
}

case class PossiblePremiseMatch(matchingPremise: Statement, substitutions: SuggestedSubstitutions)
