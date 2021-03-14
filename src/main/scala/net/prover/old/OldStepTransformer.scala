package net.prover.old

import net.prover.core.transformers.ContextWithExternalDepth
import net.prover.model.proof.{Premise, Step}
import net.prover.old.OldExpressionTransformer.TraversableExpressionTransformer
import scalaz.syntax.monad._

trait OldStepTransformer[TOutput[+_], TParameters] extends TraversableExpressionTransformer[TOutput, TParameters] {
    def transformPremise(premise: Premise, parameters: TParameters): TOutput[Premise] = {
      for {
        newStatement <- transformStatementWithoutContext(premise.statement, parameters)
      } yield Premise.Pending(newStatement)
    }
    def transformStepWithContext(
      step: Step,
      parameterConstructor: ContextWithExternalDepth => TParameters)(
      implicit contextWithExternalDepth: ContextWithExternalDepth
    ): TOutput[Step] = step match {
      case step: Step.Deduction =>
        for {
          newAssumption <- transformStatementWithoutContext(step.assumption, parameterConstructor(contextWithExternalDepth))
          newSubsteps <- traverse(step.substeps.map(transformStepWithContext(_, parameterConstructor)))
        } yield Step.Deduction(newAssumption, newSubsteps, step.deductionDefinition)
      case step: Step.Naming =>
        for {
          newAssumption <- transformStatementWithoutContext(step.assumption, parameterConstructor(contextWithExternalDepth.increaseDepth()))
          newStatement <- transformStatementWithoutContext(step.statement, parameterConstructor(contextWithExternalDepth))
          newSubsteps <- traverse(step.substeps.map(transformStepWithContext(_, parameterConstructor)(contextWithExternalDepth.increaseDepth())))
          newPremises <- traverse(step.premises.map(transformPremise(_, parameterConstructor(contextWithExternalDepth))))
          newSubstitutions <- transformSubstitutions(step.substitutions, parameterConstructor(contextWithExternalDepth))
        } yield Step.Naming(step.variableName, newAssumption, newStatement, newSubsteps, step.inference, newPremises, newSubstitutions, step.generalizationDefinition, step.deductionDefinition)
      case step: Step.Generalization =>
        for {
          newSubsteps <- traverse(step.substeps.map(transformStepWithContext(_, parameterConstructor)(contextWithExternalDepth.increaseDepth())))
        } yield Step.Generalization(step.variableName, newSubsteps, step.generalizationDefinition)
      case step: Step.Target =>
        for {
          newStatement <- transformStatementWithoutContext(step.statement, parameterConstructor(contextWithExternalDepth))
        } yield Step.Target(newStatement)
      case step: Step.Elided =>
        for {
          newSubsteps <- traverse(step.substeps.map(transformStepWithContext(_, parameterConstructor)(contextWithExternalDepth.increaseDepth())))
        } yield Step.Elided(newSubsteps, step.highlightedInference, step.description)
      case step: Step.Assertion =>
        for {
          newStatement <- transformStatementWithoutContext(step.statement, parameterConstructor(contextWithExternalDepth))
          newPremises <- traverse(step.premises.map(transformPremise(_, parameterConstructor(contextWithExternalDepth))))
          newSubstitutions <- transformSubstitutions(step.substitutions, parameterConstructor(contextWithExternalDepth))
        } yield Step.Assertion(newStatement, step.inference, newPremises, newSubstitutions)
      case step: Step.SubProof =>
        for {
          newSubsteps <- traverse(step.substeps.map(transformStepWithContext(_, parameterConstructor)(contextWithExternalDepth.increaseDepth())))
        } yield Step.SubProof(step.name, newSubsteps)
    }
  }
