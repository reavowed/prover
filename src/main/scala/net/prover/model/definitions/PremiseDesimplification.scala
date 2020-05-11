package net.prover.model.definitions

import net.prover.model.expressions.Statement
import net.prover.model.proof.{DerivationStep, Step, StepProvingContext}
import net.prover.model.{Inference, Substitutions}

sealed trait PremiseDesimplification {
  def getRootPremises: Seq[Statement]
  def getSubstitutedPremises(substitutions: Substitutions)(implicit stepProvingContext: StepProvingContext): Option[(Seq[Statement], Seq[DerivationStep])]
}
object PremiseDesimplification {
  implicit class SeqOps(premiseDesimplifications: Seq[PremiseDesimplification]) {
    def getSubstitutedPremises(substitutions: Substitutions)(implicit stepProvingContext: StepProvingContext): Option[(Seq[Statement], Seq[DerivationStep])] = {
      for {
        innerPremisesAndSteps <- premiseDesimplifications.map(_.getSubstitutedPremises(substitutions)).traverseOption
        innerPremises = innerPremisesAndSteps.flatMap(_._1)
        innerSteps = innerPremisesAndSteps.flatMap(_._2)
      } yield (innerPremises, innerSteps)
    }
  }
}

case class DirectPremise(premise: Statement) extends PremiseDesimplification {
  def getRootPremises: Seq[Statement] = Seq(premise)
  def getSubstitutedPremises(substitutions: Substitutions)(implicit stepProvingContext: StepProvingContext): Option[(Seq[Statement], Seq[DerivationStep])] = {
    for {
      substitutedPremise <- premise.applySubstitutions(substitutions)
    } yield (Seq(substitutedPremise), Nil)
  }
}
case class DesimplifiedPremise(premise: Statement, inference: Inference, innerPremiseDesimplifications: Seq[PremiseDesimplification]) extends PremiseDesimplification {
  def getRootPremises: Seq[Statement] = innerPremiseDesimplifications.flatMap(_.getRootPremises)
  def getSubstitutedPremises(substitutions: Substitutions)(implicit stepProvingContext: StepProvingContext): Option[(Seq[Statement], Seq[DerivationStep])] = {
    for {
      substitutedPremise <- premise.applySubstitutions(substitutions)
      inferenceSubstitutions <- inference.conclusion.calculateSubstitutions(substitutedPremise).flatMap(_.confirmTotality)
      assertionStep <- Step.Assertion.forInference(inference, inferenceSubstitutions)
      (innerPremises, innerSteps) <- innerPremiseDesimplifications.getSubstitutedPremises(substitutions)
    } yield (innerPremises, innerSteps :+ DerivationStep.fromAssertion(assertionStep))
  }
}
