package net.prover.theorems

import net.prover.model.definitions.ExpressionDefinition
import net.prover.model.expressions.Statement
import net.prover.model.proof.Premise
import net.prover.model.{Inference, Substitutions}
import net.prover.proving.structure.definitions.{DeductionDefinition, GeneralizationDefinition}
import net.prover.theorems.steps.RecursiveStepFinder
import scalaz.Scalaz._

object GetReferencedDefinitions extends RecursiveStepFinder[Set[ExpressionDefinition]] {
  override def apply(statement: Statement): Set[ExpressionDefinition] = statement.referencedDefinitions
  override def apply(inference: Inference.Summary): Set[ExpressionDefinition] = Set.empty
  override def apply(premise: Premise): Set[ExpressionDefinition] = Set.empty
  override def apply(substitutions: Substitutions): Set[ExpressionDefinition] = Set.empty
  override def apply(deductionDefinition: DeductionDefinition): Set[ExpressionDefinition] = Set(deductionDefinition.statementDefinition)
  override def apply(generalizationDefinition: GeneralizationDefinition): Set[ExpressionDefinition] = Set(generalizationDefinition.statementDefinition)
}
