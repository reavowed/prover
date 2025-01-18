package net.prover.theorems

import net.prover.entries.StepWithContext
import net.prover.model.{Inference, Substitutions}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, StepReference}
import net.prover.model.proof.Premise.SingleLinePremise
import net.prover.proving.structure.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.theorems.steps.RecursiveStepFinder
import scalaz.Scalaz._

object GetReferencedPremises extends RecursiveStepFinder[List[Premise]] {
  def apply(stepWithContext: StepWithContext): List[Premise] = {
    apply(stepWithContext.step).filter {
      p => !p.asOptionalInstanceOf[SingleLinePremise]
        .flatMap(_.referencedLine.asOptionalInstanceOf[StepReference])
        .exists(_.stepPath.startsWith(stepWithContext.stepContext.stepReference.stepPath))
    }
  }
  override def apply(statement: Statement): List[Premise] = Nil
  override def apply(inference: Inference.Summary): List[Premise] = Nil
  override def apply(premise: Premise): List[Premise] = List(premise)
  override def apply(substitutions: Substitutions): List[Premise] = Nil
  override def apply(deductionDefinition: DeductionDefinition): List[Premise] = Nil
  override def apply(generalizationDefinition: GeneralizationDefinition): List[Premise] = Nil
}
