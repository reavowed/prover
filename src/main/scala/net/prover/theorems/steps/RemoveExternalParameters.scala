package net.prover.theorems.steps

import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step}
import net.prover.model.{Inference, Substitutions}
import scalaz.Scalaz._

object RemoveExternalParameters extends StepUpdater[Int, Option] {
  def apply(steps: Seq[Step], numberOfParametersToRemove: Int): Option[Seq[Step]] = {
    apply(steps.toList, numberOfParametersToRemove, Nil)
  }

  override def updateStatement(
    statement: Statement,
    numberOfParametersToRemove: Int,
    boundVariableNames: List[List[String]]
  ): Option[Statement] = {
    statement.removeExternalParameters(numberOfParametersToRemove, boundVariableNames.length)
  }

  override def updateInference(
    inference: Inference.Summary,
    numberOfParametersToRemove: Int,
    boundVariableNames: List[List[String]]
  ): Option[Inference.Summary] = {
    Some(inference)
  }

  override def updatePremise(
    premise: Premise,
    numberOfParametersToRemove: Int,
    boundVariableNames: List[List[String]]
  ): Option[Premise] = {
    premise.removeExternalParameters(numberOfParametersToRemove, boundVariableNames.length)
  }

  override def updateSubstitutions(
    substitutions: Substitutions,
    numberOfParametersToRemove: Int,
    boundVariableNames: List[List[String]]
  ): Option[Substitutions] = {
    substitutions.removeExternalParameters(numberOfParametersToRemove, boundVariableNames.length)
  }
}
