package net.prover.theorems

import net.prover.entries.StepWithContext
import net.prover.model.{Inference, Substitutions}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{InternalStepReference, Premise, StepContext, StepReference}
import net.prover.model.proof.Premise.SingleLinePremise
import net.prover.proving.structure.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.theorems.steps.RecursiveStepFinder
import scalaz.Scalaz._

object GetReferencedPremises extends RecursiveStepFinder[List[Premise]] {
  def apply(stepWithContext: StepWithContext): List[Premise] = {
    removeInternalPremises(
      apply(stepWithContext.step),
      stepWithContext.stepContext)
  }
  override def apply(statement: Statement): List[Premise] = Nil
  override def apply(inference: Inference.Summary): List[Premise] = Nil
  override def apply(premise: Premise): List[Premise] = List(premise)
  override def apply(substitutions: Substitutions): List[Premise] = Nil
  override def apply(deductionDefinition: DeductionDefinition): List[Premise] = Nil
  override def apply(generalizationDefinition: GeneralizationDefinition): List[Premise] = Nil

  def removeInternalPremises(premises: List[Premise], stepContext: StepContext): List[Premise] = {
    def getStepPath(p: SingleLinePremise): Option[Seq[Int]] = p.referencedLine match {
      case StepReference(stepPath) =>
        Some(stepPath)
      case InternalStepReference(stepPath, _) =>
        Some(stepPath)
      case _ =>
        None
    }
    premises.filter {
      p => !p.asOptionalInstanceOf[SingleLinePremise]
        .flatMap(getStepPath)
        .exists(_.startsWith(stepContext.stepReference.stepPath))
    }
  }
}
