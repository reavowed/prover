package net.prover.theorems

import net.prover.model.expressions.Statement
import net.prover.model.proof.{Premise, Step}
import net.prover.model.proof.Premise.Simplification
import net.prover.model.{Inference, Substitutions}
import net.prover.util.FunctorTypes.Identity

object ClearInference extends CompoundTheoremUpdater[Inference, Identity] {

  override def updateNaming(
    step: Step.Naming,
    inferenceToClear: Inference,
    boundVariableNames: List[List[String]]
  ): Step = {
    if (inferenceToClear == step.inference) {
      Step.Target(step.statement)
    } else {
      super.updateNaming(step, inferenceToClear, boundVariableNames)
    }
  }

  override def updateElided(
    step: Step.Elided,
    inferenceToClear: Inference,
    boundVariableNames: List[List[String]]
  ): Step = {
    if (step.highlightedInference.contains(inferenceToClear)) {
      Step.Target(step.provenStatement.get)
    } else {
      super.updateElided(step, inferenceToClear, boundVariableNames)
    }
  }

  override def updateAssertion(
    step: Step.Assertion,
    inferenceToClear: Inference,
    boundVariableNames: List[List[String]]
  ): Step = {
    if (inferenceToClear == step.inference) {
      Step.Target(step.statement)
    } else {
      super.updateAssertion(step, inferenceToClear, boundVariableNames)
    }
  }


  override def updateStatement(
    statement: Statement,
    inferenceToClear: Inference,
    boundVariableNames: List[List[String]]
  ): Statement = {
    statement
  }

  override def updateInference(
    inference: Inference.Summary,
    inferenceToClear: Inference,
    boundVariableNames: List[List[String]]
  ): Inference.Summary = {
    inference
  }

  override def updatePremise(
    premise: Premise,
    inferenceToClear: Inference,
    boundVariableNames: List[List[String]]
  ): Premise = {
    premise match {
      case Simplification(statement, _, `inferenceToClear`, _, _) =>
        Premise.Pending(statement)
      case Simplification(statement, inner, _, _, _) if updatePremise(inner, inferenceToClear, boundVariableNames) != inner =>
        Premise.Pending(statement)
      case premise =>
        premise
    }
  }

  override def updateSubstitutions(
    substitutions: Substitutions,
    inferenceToClear: Inference,
    boundVariableNames: List[List[String]]
  ): Substitutions = {
    substitutions
  }
}
