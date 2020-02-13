package net.prover.controllers.models

import net.prover.model.{Inference, Substitutions}
import net.prover.model.expressions.Statement
import net.prover.model.proof.StepProvingContext
import net.prover.model.proof.SubstatementExtractor.ExtractionOption

case class PossibleInference(inference: Inference.Summary, possibleConclusions: Seq[PossibleConclusion]) {
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

case class PossibleConclusion(
  conclusion: Statement,
  possiblePremises: Seq[PossiblePremise],
  substitutions: Option[SuggestedSubstitutions],
  requiredSubstitutions: Substitutions.Required,
  extractionInferenceIds: Seq[String])

object PossibleConclusion {
  def fromExtractionOptionWithTarget(extractionOption: ExtractionOption, target: Statement)(implicit stepProvingContext: StepProvingContext): Option[PossibleConclusion] = {
    fromExtractionOptionWithSubstitutions(extractionOption, _.calculateSubstitutions(target))
  }
  def fromExtractionOptionWithSubstitutions(extractionOption: ExtractionOption, getSubstitutions: Statement => Option[Substitutions.Possible])(implicit stepProvingContext: StepProvingContext): Option[PossibleConclusion] = {
    getSubstitutions(extractionOption.conclusion).map(s => fromExtractionOption(extractionOption, Some(s)))
  }
  def fromExtractionOption(extractionOption: ExtractionOption, substitutions: Option[Substitutions.Possible])(implicit stepProvingContext: StepProvingContext): PossibleConclusion = {
    PossibleConclusion(
      extractionOption.conclusion,
      PossiblePremise.fromAvailablePremises(extractionOption.premises, substitutions),
      substitutions.map(SuggestedSubstitutions(_)),
      extractionOption.requiredSubstitutions,
      extractionOption.extractionInferences.map(_.id))
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
      val matches = stepProvingContext.allPremises.map(_.statement).mapCollect { availablePremise =>
        premise.calculateSubstitutions(availablePremise, substitutions.getOrElse(Substitutions.Possible.empty))
          .map(s => PossiblePremiseMatch(availablePremise, SuggestedSubstitutions(s)))
      }
      PossiblePremise(premise, matches)
    }
  }
}

case class PossiblePremiseMatch(matchingPremise: Statement, substitutions: SuggestedSubstitutions)
