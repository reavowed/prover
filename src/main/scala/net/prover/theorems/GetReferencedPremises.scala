package net.prover.theorems

import net.prover.model.{Inference, Substitutions}
import net.prover.model.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.model.expressions.Statement
import net.prover.model.proof.Premise
import net.prover.theorems.steps.RecursiveStepFinder
import scalaz.Scalaz._

object GetAllPremises extends RecursiveStepFinder[List[Premise]] {
  override def apply(statement: Statement): List[Premise] = Nil
  override def apply(inference: Inference.Summary): List[Premise] = Nil
  override def apply(premise: Premise): List[Premise] = List(premise)
  override def apply(substitutions: Substitutions): List[Premise] = Nil
  override def apply(deductionDefinition: DeductionDefinition): List[Premise] = Nil
  override def apply(generalizationDefinition: GeneralizationDefinition): List[Premise] = Nil
}
