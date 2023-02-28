package net.prover.proving.premiseFinding

import net.prover.model.expressions.Statement
import net.prover.model.proof.{DerivationStep, EqualityRewriter, Step, StepProvingContext}
import net.prover.model.utils.ExpressionUtils

object DerivationOrTargetFinder {
  private def findDerivationsOrTargets(
    premiseStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivationStep], Seq[Step.Target]) = {
    findDerivationsOrTargetsWithSuccessResult(premiseStatement).strip3
  }

  private def findDerivationsOrTargetsWithSuccessResult(
    premiseStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivationStep], Seq[Step.Target], Boolean) = {
    val directly = DerivationFinder.findDerivationForStatement(premiseStatement).map((_, Nil, true))

    def byDeconstructing = for {
      (step, deconstructedStatements) <- deconstruct(premiseStatement)
      (innerSteps, innerTargets, wasSuccessful) = findDerivationsOrTargetsWithSuccessResult(deconstructedStatements)
      if wasSuccessful
    } yield (innerSteps :+ step, innerTargets, true)

    def asTarget = {
      val (rewriteSteps, rewrittenStatement) = rewriteTarget(premiseStatement)
      val (deconstructionSteps, deconstructedStatements) = splitTarget(rewrittenStatement)
      (rewriteSteps ++ deconstructionSteps, deconstructedStatements.map(Step.Target(_)), false)
    }

    directly orElse byDeconstructing getOrElse asTarget
  }

  def findDerivationsOrTargets(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivationStep], Seq[Step.Target]) = {
    val (premiseSteps, targets) = premiseStatements.foldLeft((Seq.empty[DerivationStep], Seq.empty[Step.Target])) { case ((premiseStepsSoFar, targetStepsSoFar), premiseStatement) =>
      val (stepsForThisPremise, targetsForThisPremise) = findDerivationsOrTargets(premiseStatement)
      (premiseStepsSoFar ++ stepsForThisPremise, targetStepsSoFar ++ targetsForThisPremise)
    }
    (premiseSteps.deduplicate, targets)
  }

  def findDerivationsOrTargetsWithSuccessResult(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivationStep], Seq[Step.Target], Boolean) = {
    val (premiseSteps, targets, wasSuccessful) = premiseStatements.foldLeft((Seq.empty[DerivationStep], Seq.empty[Step.Target], false)) { case ((premiseStepsSoFar, targetStepsSoFar, wasSuccessfulSoFar), premiseStatement) =>
      val (stepsForThisPremise, targetsForThisPremise, wasThisStatementSuccessful) = findDerivationsOrTargetsWithSuccessResult(premiseStatement)
      (premiseStepsSoFar ++ stepsForThisPremise, targetStepsSoFar ++ targetsForThisPremise, wasSuccessfulSoFar || wasThisStatementSuccessful)
    }
    (premiseSteps.deduplicate, targets, wasSuccessful)
  }

  private def splitTarget(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivationStep], Seq[Statement]) = {
    def default = (Nil, Seq(targetStatement))

    if (ExpressionUtils.isTypeLikeStatement(targetStatement)) {
      default
    } else {
      deconstruct(targetStatement) match {
        case Some((deconstructionStep, innerTargets)) => splitTargets(innerTargets).mapLeft(deconstructionStep +: _)
        case None => default
      }
    }
  }

  private def splitTargets(
    targetStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivationStep], Seq[Statement]) = {
    targetStatements.map(splitTarget).splitFlatten
  }

  private def rewriteTarget(
    premiseStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivationStep], Statement) = {
    stepProvingContext.knownValuesToProperties.foldLeft((premiseStatement, Seq.empty[DerivationStep])) { case ((currentStatement, currentDerivation), propertyValue) =>
      EqualityRewriter.getReverseReplacements(currentStatement, propertyValue.lhs, propertyValue.rhs, propertyValue.equality) match {
        case Some((result, derivationStep)) =>
          (result, currentDerivation ++ propertyValue.derivation :+ derivationStep)
        case None =>
          (currentStatement, currentDerivation)
      }
    }.swap
  }

  private def deconstruct(
    statement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[(DerivationStep, Seq[Statement])] = {
    stepProvingContext.provingContext.statementDefinitionDeconstructions.mapFind { deconstructionInference =>
      for {
        substitutions <- deconstructionInference.conclusion.calculateSubstitutions(statement).flatMap(_.confirmTotality(deconstructionInference.variableDefinitions))
        step <- Step.Assertion.forInference(deconstructionInference, substitutions)
      } yield (DerivationStep.fromAssertion(step), step.premises.map(_.statement))
    }
  }
}