package net.prover.proving.premiseFinding

import net.prover.model.*
import net.prover.model.definitions.KnownStatement
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepProvingContext}
import net.prover.model.utils.ExpressionUtils
import net.prover.proving.derivation.SimpleDerivation

object DerivationOrTargetFinder {
  def findDerivationsOrTargets(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[KnownStatement], Seq[Step.TargetStep]) = {
    findDerivationsOrTargetsWithSuccessResult(premiseStatements).strip3
  }

  private def findDerivationsOrTargetsWithSuccessResult(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[KnownStatement], Seq[Step.TargetStep], Boolean) = {
    premiseStatements.foldLeft((Seq.empty[KnownStatement], Seq.empty[Step.TargetStep], false)) { case ((knownStatementsSoFar, targetStepsSoFar, wasSuccessfulSoFar), premiseStatement) =>
      val (knownStatement, targetsForThisPremise, wasThisStatementSuccessful) = findDerivationOrTargetsWithSuccessResult(premiseStatement)
      (knownStatementsSoFar :+ knownStatement, targetStepsSoFar ++ targetsForThisPremise, wasSuccessfulSoFar || wasThisStatementSuccessful)
    }
  }

  private def findDerivationOrTargetsWithSuccessResult(
    premiseStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (KnownStatement, Seq[Step.TargetStep], Boolean) = {
    val directly = DerivationFinder.findDerivationForUnwrappedStatement(premiseStatement).map((_, Nil, true))

    def byDeconstructing = for {
      (step, deconstructedStatements) <- deconstruct(premiseStatement)
      (innerKnown, innerTargets, wasSuccessful) = findDerivationsOrTargetsWithSuccessResult(deconstructedStatements)
      if wasSuccessful
      combinedDerivation = SimpleDerivation(innerKnown.flatMap(_.derivation.steps)) :+ step
    } yield (combinedDerivation, innerTargets, true)

    def asTarget = {
      val (rewriteSteps, rewrittenStatement) = DerivationFinder.rewriteWithKnownValues(premiseStatement)
      val (deconstructionSteps, deconstructedStatements) = splitTarget(rewrittenStatement)
      (rewriteSteps ++ deconstructionSteps, deconstructedStatements.map(Step.TargetStep(_)), false)
    }

    (directly orElse byDeconstructing getOrElse asTarget).map1(d => KnownStatement(premiseStatement, d))
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
