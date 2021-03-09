package net.prover.model.definitions

import net.prover.model.expressions.Statement
import net.prover.model.proof.{DerivationStep, Step, StepProvingContext}
import net.prover.model.{Inference, Substitutions}
import net.prover.old.OldSubstitutionApplier
import net.prover.substitutionFinding.transformers.PossibleSubstitutionCalculator

sealed trait DerivedPremise {
  def getRootPremises: Seq[Statement]
  def getSubstitutedPremises(substitutions: Substitutions)(implicit stepProvingContext: StepProvingContext): Option[(Seq[Statement], Seq[DerivationStep])]
}
object DerivedPremise {
  implicit class SeqOps(premiseDesimplifications: Seq[DerivedPremise]) {
    def getSubstitutedPremises(substitutions: Substitutions)(implicit stepProvingContext: StepProvingContext): Option[(Seq[Statement], Seq[DerivationStep])] = {
      for {
        innerPremisesAndSteps <- premiseDesimplifications.map(_.getSubstitutedPremises(substitutions)).traverseOption
        innerPremises = innerPremisesAndSteps.flatMap(_._1)
        innerSteps = innerPremisesAndSteps.flatMap(_._2)
      } yield (innerPremises, innerSteps)
    }
  }
}

case class DirectPremise(premise: Statement) extends DerivedPremise {
  def getRootPremises: Seq[Statement] = Seq(premise)
  def getSubstitutedPremises(substitutions: Substitutions)(implicit stepProvingContext: StepProvingContext): Option[(Seq[Statement], Seq[DerivationStep])] = {
    for {
      substitutedPremise <- OldSubstitutionApplier.applySubstitutions(premise, substitutions).toOption
    } yield (Seq(substitutedPremise), Nil)
  }
}
case class DesimplifiedPremise(premise: Statement, inference: Inference, innerPremises: Seq[DerivedPremise]) extends DerivedPremise {
  def getRootPremises: Seq[Statement] = innerPremises.flatMap(_.getRootPremises)
  def getSubstitutedPremises(substitutions: Substitutions)(implicit stepProvingContext: StepProvingContext): Option[(Seq[Statement], Seq[DerivationStep])] = {
    for {
      substitutedPremise <- OldSubstitutionApplier.applySubstitutions(premise, substitutions).toOption
      inferenceSubstitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(inference.conclusion, substitutedPremise).flatMap(_.confirmTotality(inference.variableDefinitions))
      assertionStep <- Step.Assertion.forInference(inference, inferenceSubstitutions)
      (innerPremises, innerSteps) <- innerPremises.getSubstitutedPremises(substitutions)
    } yield (innerPremises, innerSteps :+ DerivationStep.fromAssertion(assertionStep))
  }
}
