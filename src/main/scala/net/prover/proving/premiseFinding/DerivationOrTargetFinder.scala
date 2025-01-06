package net.prover.proving.premiseFinding

import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepProvingContext}
import net.prover.model.utils.ExpressionUtils
import net.prover.proving.derivation.SimpleDerivation

object DerivationOrTargetFinder {
  def findDerivationsOrTargets(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (SimpleDerivation, Seq[Step.TargetStep]) = {
    findDerivationsOrTargetsWithSuccessResult(premiseStatements).strip3
  }

  private def findDerivationsOrTargetsWithSuccessResult(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (SimpleDerivation, Seq[Step.TargetStep], Boolean) = {
    val (premiseSteps, targets, wasSuccessful) = premiseStatements.foldLeft((SimpleDerivation.empty, Seq.empty[Step.TargetStep], false)) { case ((premiseStepsSoFar, targetStepsSoFar, wasSuccessfulSoFar), premiseStatement) =>
      val (stepsForThisPremise, targetsForThisPremise, wasThisStatementSuccessful) = findDerivationsOrTargetsWithSuccessResult(premiseStatement)
      (premiseStepsSoFar ++ stepsForThisPremise, targetStepsSoFar ++ targetsForThisPremise, wasSuccessfulSoFar || wasThisStatementSuccessful)
    }
    (premiseSteps.distinct, targets, wasSuccessful)
  }

  private def findDerivationsOrTargets(
    premiseStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (SimpleDerivation, Seq[Step.TargetStep]) = {
    findDerivationsOrTargetsWithSuccessResult(premiseStatement).strip3
  }

  private def findDerivationsOrTargetsWithSuccessResult(
    premiseStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (SimpleDerivation, Seq[Step.TargetStep], Boolean) = {
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

  private def splitTarget(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (SimpleDerivation, Seq[Statement]) = {
    def default = (SimpleDerivation.empty, Seq(targetStatement))

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
  ): (SimpleDerivation, Seq[Statement]) = {
    targetStatements.map(splitTarget).split.mapLeft(_.join).mapRight(_.flatten)
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
