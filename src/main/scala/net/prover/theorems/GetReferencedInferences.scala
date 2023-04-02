package net.prover.theorems

import net.prover.entries.{EntryWithContext, StepWithContext, TheoremWithContext}
import net.prover.model.Inference
import net.prover.model.entries.Theorem
import net.prover.model.proof.Step
import net.prover.theorems.steps.RecursiveStepFinder
import scalaz.Scalaz._

object GetReferencedInferences extends RecursiveStepFinder[Set[Inference]] {
  def apply(entryWithContext: EntryWithContext): Set[Inference] = {
    if (entryWithContext.entry.isInstanceOf[Theorem]) {
      entryWithContext.asInstanceOf[TheoremWithContext].theorem.proofs.flatMap(p => apply(p.steps)).toSet
    } else {
      Set.empty
    }
  }

  override def getFromTarget(step: Step.Target): Set[Inference] = {
    Set.empty
  }
  override def getFromAssertion(step: Step.Assertion): Set[Inference] = {
    step.premises.flatMap(_.referencedInferences).toSet + step.inference
  }
  override def getFromDeduction(step: Step.Deduction): Set[Inference] = {
    apply(step.substeps)
  }
  override def getFromGeneralization(step: Step.Generalization): Set[Inference] = {
    apply(step.substeps)
  }
  override def getFromNaming(step: Step.Naming): Set[Inference] = {
    apply(step.substeps) ++ step.premises.flatMap(_.referencedInferences) + step.inference
  }
  override def getFromSubProof(step: Step.SubProof): Set[Inference] = {
    apply(step.substeps)
  }
  override def getFromElided(step: Step.Elided): Set[Inference] = {
    apply(step.substeps)
  }
  override def getFromExistingStatementExtraction(step: Step.ExistingStatementExtraction): Set[Inference] = {
    apply(step.substeps)
  }
}
