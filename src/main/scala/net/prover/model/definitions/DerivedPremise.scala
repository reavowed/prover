package net.prover.model.definitions

import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, SubstitutionContext}
import net.prover.model.{Inference, Substitutions}

sealed trait DerivedPremise {
  def getRootPremises: Seq[Statement]
  def getSubstitutedPremises(substitutions: Substitutions)(implicit substitutionContext: SubstitutionContext): Option[(Seq[Statement], Seq[Step.AssertionStep])]
}
object DerivedPremise {
  implicit class SeqOps(premiseDesimplifications: Seq[DerivedPremise]) {
    def getSubstitutedPremises(substitutions: Substitutions)(implicit substitutionContext: SubstitutionContext): Option[(Seq[Statement], Seq[Step.AssertionStep])] = {
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
  def getSubstitutedPremises(substitutions: Substitutions)(implicit substitutionContext: SubstitutionContext): Option[(Seq[Statement], Seq[Step.AssertionStep])] = {
    for {
      substitutedPremise <- premise.applySubstitutions(substitutions)
    } yield (Seq(substitutedPremise), Nil)
  }
}
case class DesimplifiedPremise(premise: Statement, inference: Inference, innerPremises: Seq[DerivedPremise]) extends DerivedPremise {
  def getRootPremises: Seq[Statement] = innerPremises.flatMap(_.getRootPremises)
  def getSubstitutedPremises(substitutions: Substitutions)(implicit substitutionContext: SubstitutionContext): Option[(Seq[Statement], Seq[Step.AssertionStep])] = {
    for {
      substitutedPremise <- premise.applySubstitutions(substitutions)
      inferenceSubstitutions <- inference.conclusion.calculateSubstitutions(substitutedPremise).flatMap(_.confirmTotality(inference.variableDefinitions))
      assertionStep <- Step.AssertionStep.forInference(inference, inferenceSubstitutions)
      (innerPremises, innerSteps) <- innerPremises.getSubstitutedPremises(substitutions)
    } yield (innerPremises, innerSteps :+ assertionStep)
  }
}
