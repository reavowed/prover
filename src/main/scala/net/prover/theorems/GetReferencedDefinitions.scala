package net.prover.theorems

import net.prover.model.definitions.ExpressionDefinition
import net.prover.model.entries.Theorem.Proof
import net.prover.model.proof.Step
import net.prover.theorems.steps.RecursiveStepFinder
import scalaz.Scalaz._

object GetReferencedDefinitions extends RecursiveStepFinder[Set[ExpressionDefinition]] {
  def apply(proof: Proof): Set[ExpressionDefinition] = {
    apply(proof.steps)
  }

  override def getFromTarget(step: Step.Target): Set[ExpressionDefinition] = {
    step.statement.referencedDefinitions
  }
  override def getFromAssertion(step: Step.Assertion): Set[ExpressionDefinition] = {
    step.statement.referencedDefinitions
  }
  override def getFromDeduction(step: Step.Deduction): Set[ExpressionDefinition] = {
    import step._
    assumption.referencedDefinitions ++ apply(substeps) + deductionDefinition.statementDefinition
  }
  override def getFromGeneralization(step: Step.Generalization): Set[ExpressionDefinition] = {
    import step._
    apply(step.substeps) + generalizationDefinition.statementDefinition
  }
  override def getFromNaming(step: Step.Naming): Set[ExpressionDefinition] = {
    import step._
    assumption.referencedDefinitions ++ apply(substeps) + deductionDefinition.statementDefinition + generalizationDefinition.statementDefinition
  }
  override def getFromSubProof(step: Step.SubProof): Set[ExpressionDefinition] = {
    apply(step.substeps)
  }
  override def getFromElided(step: Step.Elided): Set[ExpressionDefinition] = {
    apply(step.substeps)
  }
  override def getFromExistingStatementExtraction(step: Step.ExistingStatementExtraction): Set[ExpressionDefinition] = {
    apply(step.substeps)
  }
}
