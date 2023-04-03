package net.prover.proving.premiseFinding

import net.prover.model.expressions.Statement
import net.prover.model.proof.{EqualityRewriter, Step, StepContext}
import net.prover.model.utils.ExpressionUtils

object DerivationOrTargetFinder {
  private def findDerivationsOrTargets(
    premiseStatement: Statement)(
    implicit stepContext: StepContext
  ): (Seq[Step.InferenceApplicationWithoutPremises], Seq[Step.Target]) = {
    findDerivationsOrTargetsWithSuccessResult(premiseStatement).strip3
  }

  private def findDerivationsOrTargetsWithSuccessResult(
    premiseStatement: Statement)(
    implicit stepContext: StepContext
  ): (Seq[Step.InferenceApplicationWithoutPremises], Seq[Step.Target], Boolean) = {
    val directly = DerivationFinder.findDerivationForUnwrappedStatement(premiseStatement).map((_, Nil, true))

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
    implicit stepContext: StepContext
  ): (Seq[Step.InferenceApplicationWithoutPremises], Seq[Step.Target]) = {
    val (premiseSteps, targets) = premiseStatements.foldLeft((Seq.empty[Step.InferenceApplicationWithoutPremises], Seq.empty[Step.Target])) { case ((premiseStepsSoFar, targetStepsSoFar), premiseStatement) =>
      val (stepsForThisPremise, targetsForThisPremise) = findDerivationsOrTargets(premiseStatement)
      (premiseStepsSoFar ++ stepsForThisPremise, targetStepsSoFar ++ targetsForThisPremise)
    }
    (premiseSteps.distinctBy(_.statement), targets)
  }

  def findDerivationsOrTargetsWithSuccessResult(
    premiseStatements: Seq[Statement])(
    implicit stepContext: StepContext
  ): (Seq[Step.InferenceApplicationWithoutPremises], Seq[Step.Target], Boolean) = {
    val (premiseSteps, targets, wasSuccessful) = premiseStatements.foldLeft((Seq.empty[Step.InferenceApplicationWithoutPremises], Seq.empty[Step.Target], false)) { case ((premiseStepsSoFar, targetStepsSoFar, wasSuccessfulSoFar), premiseStatement) =>
      val (stepsForThisPremise, targetsForThisPremise, wasThisStatementSuccessful) = findDerivationsOrTargetsWithSuccessResult(premiseStatement)
      (premiseStepsSoFar ++ stepsForThisPremise, targetStepsSoFar ++ targetsForThisPremise, wasSuccessfulSoFar || wasThisStatementSuccessful)
    }
    (premiseSteps.distinctBy(_.statement), targets, wasSuccessful)
  }

  private def splitTarget(
    targetStatement: Statement)(
    implicit stepContext: StepContext
  ): (Seq[Step.InferenceApplicationWithoutPremises], Seq[Statement]) = {
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
    implicit stepContext: StepContext
  ): (Seq[Step.InferenceApplicationWithoutPremises], Seq[Statement]) = {
    targetStatements.map(splitTarget).splitFlatten
  }

  private def rewriteTarget(
    premiseStatement: Statement)(
    implicit stepContext: StepContext
  ): (Seq[Step.InferenceApplicationWithoutPremises], Statement) = {
    stepContext.knownValuesToProperties.foldLeft((premiseStatement, Seq.empty[Step.InferenceApplicationWithoutPremises])) { case ((currentStatement, currentDerivation), propertyValue) =>
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
    implicit stepContext: StepContext
  ): Option[(Step.Assertion, Seq[Statement])] = {
    stepContext.provingContext.statementDefinitionDeconstructions.mapFind { deconstructionInference =>
      for {
        substitutions <- deconstructionInference.conclusion.calculateSubstitutions(statement).flatMap(_.confirmTotality(deconstructionInference.variableDefinitions))
        step <- Step.Assertion.forInference(deconstructionInference, substitutions)
      } yield (step, step.premises.map(_.statement))
    }
  }
}
