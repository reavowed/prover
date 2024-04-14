package net.prover.proving.premiseFinding

import net.prover.model.expressions.Statement
import net.prover.model.proof.{EqualityRewriter, Step, StepProvingContext}
import net.prover.model.utils.ExpressionUtils

object DerivationOrTargetFinder {
  private def findDerivationsOrTargets(
    premiseStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[Step.AssertionOrExtraction], Seq[Step.TargetStep]) = {
    findDerivationsOrTargetsWithSuccessResult(premiseStatement).strip3
  }

  private def findDerivationsOrTargetsWithSuccessResult(
    premiseStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[Step.AssertionOrExtraction], Seq[Step.TargetStep], Boolean) = {
    val directly = DerivationFinder.findDerivationForUnwrappedStatement(premiseStatement).map((_, Nil, true))

    def byDeconstructing = for {
      (step, deconstructedStatements) <- deconstruct(premiseStatement)
      (innerSteps, innerTargets, wasSuccessful) = findDerivationsOrTargetsWithSuccessResult(deconstructedStatements)
      if wasSuccessful
    } yield (innerSteps :+ step, innerTargets, true)

    def asTarget = {
      val (rewriteSteps, rewrittenStatement) = DerivationFinder.rewriteWithKnownValues(premiseStatement)
      val (deconstructionSteps, deconstructedStatements) = splitTarget(rewrittenStatement)
      (rewriteSteps ++ deconstructionSteps, deconstructedStatements.map(Step.TargetStep(_)), false)
    }

    directly orElse byDeconstructing getOrElse asTarget
  }

  def findDerivationsOrTargets(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[Step.AssertionOrExtraction], Seq[Step.TargetStep]) = {
    val (premiseSteps, targets) = premiseStatements.foldLeft((Seq.empty[Step.AssertionOrExtraction], Seq.empty[Step.TargetStep])) { case ((premiseStepsSoFar, targetStepsSoFar), premiseStatement) =>
      val (stepsForThisPremise, targetsForThisPremise) = findDerivationsOrTargets(premiseStatement)
      (premiseStepsSoFar ++ stepsForThisPremise, targetStepsSoFar ++ targetsForThisPremise)
    }
    (premiseSteps.distinctBy(_.statement), targets)
  }

  def findDerivationsOrTargetsWithSuccessResult(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[Step.AssertionOrExtraction], Seq[Step.TargetStep], Boolean) = {
    val (premiseSteps, targets, wasSuccessful) = premiseStatements.foldLeft((Seq.empty[Step.AssertionOrExtraction], Seq.empty[Step.TargetStep], false)) { case ((premiseStepsSoFar, targetStepsSoFar, wasSuccessfulSoFar), premiseStatement) =>
      val (stepsForThisPremise, targetsForThisPremise, wasThisStatementSuccessful) = findDerivationsOrTargetsWithSuccessResult(premiseStatement)
      (premiseStepsSoFar ++ stepsForThisPremise, targetStepsSoFar ++ targetsForThisPremise, wasSuccessfulSoFar || wasThisStatementSuccessful)
    }
    (premiseSteps.distinctBy(_.statement), targets, wasSuccessful)
  }

  private def splitTarget(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[Step.AssertionOrExtraction], Seq[Statement]) = {
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
  ): (Seq[Step.AssertionOrExtraction], Seq[Statement]) = {
    targetStatements.map(splitTarget).splitFlatten
  }

  private def deconstruct(
    statement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[(Step.AssertionStep, Seq[Statement])] = {
    stepProvingContext.provingContext.statementDefinitionDeconstructions.mapFind { deconstructionInference =>
      for {
        substitutions <- deconstructionInference.conclusion.calculateSubstitutions(statement).flatMap(_.confirmTotality(deconstructionInference.variableDefinitions))
        step <- Step.AssertionStep.forInference(deconstructionInference, substitutions)
      } yield (step, step.premises.map(_.statement))
    }
  }
}
