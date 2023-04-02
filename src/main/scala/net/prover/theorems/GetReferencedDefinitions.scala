package net.prover.theorems

import net.prover.model.definitions.{DeductionDefinition, ExpressionDefinition, GeneralizationDefinition}
import net.prover.model.entries.Theorem.Proof
import net.prover.model.expressions.Statement
import net.prover.model.proof.Premise
import net.prover.model.{Inference, Substitutions}
import net.prover.theorems.steps.RecursiveStepFinder
import scalaz.Scalaz._

object GetReferencedDefinitions extends RecursiveStepFinder[Set[ExpressionDefinition]] {
  def apply(proof: Proof): Set[ExpressionDefinition] = {
    apply(proof.steps)
  }

  override def apply(statement: Statement): Set[ExpressionDefinition] = statement.referencedDefinitions
  override def apply(inference: Inference.Summary): Set[ExpressionDefinition] = Set.empty
  override def apply(premise: Premise): Set[ExpressionDefinition] = Set.empty
  override def apply(substitutions: Substitutions): Set[ExpressionDefinition] = Set.empty
  override def apply(deductionDefinition: DeductionDefinition): Set[ExpressionDefinition] = Set(deductionDefinition.statementDefinition)
  override def apply(generalizationDefinition: GeneralizationDefinition): Set[ExpressionDefinition] = Set(generalizationDefinition.statementDefinition)

}
