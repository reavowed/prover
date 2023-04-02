package net.prover.theorems

import net.prover.model.{Inference, Substitutions}
import net.prover.model.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.model.expressions.Statement
import net.prover.model.proof.Premise
import net.prover.theorems.steps.RecursiveStepFinder
import scalaz.Scalaz._

object GetAllPremises extends RecursiveStepFinder[Set[Premise]] {
  override def apply(statement: Statement): Set[Premise] = Set.empty
  override def apply(inference: Inference.Summary): Set[Premise] = Set.empty
  override def apply(premise: Premise): Set[Premise] = Set(premise)
  override def apply(substitutions: Substitutions): Set[Premise] = Set.empty
  override def apply(deductionDefinition: DeductionDefinition): Set[Premise] = Set.empty
  override def apply(generalizationDefinition: GeneralizationDefinition): Set[Premise] = Set.empty
}
