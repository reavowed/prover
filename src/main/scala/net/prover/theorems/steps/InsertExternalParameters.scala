package net.prover.theorems.steps

import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step}
import net.prover.model.{Inference, Substitutions}
import net.prover.util.FunctorTypes.Identity

object InsertExternalParameters extends StepUpdater[Int, Identity] {

  def apply(steps: Seq[Step], numberOfParametersToInsert: Int): Seq[Step] = {
    apply(steps.toList, numberOfParametersToInsert, Nil)
  }

  override def updateStatement(
    statement: Statement,
    numberOfParametersToInsert: Int,
    boundVariableNames: List[List[String]]
  ): Statement = {
    statement.insertExternalParameters(numberOfParametersToInsert, boundVariableNames.length)
  }

  override def updateInference(
    inference: Inference.Summary,
    numberOfParametersToInsert: Int,
    boundVariableNames: List[List[String]]
  ): Inference.Summary = {
    inference
  }

  override def updatePremise(
    premise: Premise,
    numberOfParametersToInsert: Int,
    boundVariableNames: List[List[String]]
  ): Premise = {
    premise.insertExternalParameters(numberOfParametersToInsert, boundVariableNames.length)
  }

  override def updateSubstitutions(
    substitutions: Substitutions,
    numberOfParametersToInsert: Int,
    boundVariableNames: List[List[String]]
  ): Substitutions = {
    substitutions.insertExternalParameters(numberOfParametersToInsert, boundVariableNames.length)
  }
}
