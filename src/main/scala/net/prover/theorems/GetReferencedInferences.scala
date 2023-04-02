package net.prover.theorems

import net.prover.entries.{EntryWithContext, StepWithContext, TheoremWithContext}
import net.prover.model.{Inference, Substitutions}
import net.prover.model.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.model.entries.Theorem
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step}
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

  override def apply(statement: Statement): Set[Inference] = Set.empty
  override def apply(inference: Inference.Summary): Set[Inference] = Set(inference)
  override def apply(premise: Premise): Set[Inference] = premise.referencedInferences
  override def apply(substitutions: Substitutions): Set[Inference] = Set.empty
  override def apply(deductionDefinition: DeductionDefinition): Set[Inference] = Set.empty
  override def apply(generalizationDefinition: GeneralizationDefinition): Set[Inference] = Set.empty
}
