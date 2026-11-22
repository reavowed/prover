package net.prover.proving.premiseFinding

import net.prover.model.*
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepProvingContext}
import net.prover.model.utils.ExpressionUtils
import net.prover.proving.derivation.DerivedStatement

object DerivationOrTargetFinder {
  def findDerivationsOrTargets(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivedStatement], Seq[Step.TargetStep]) = {
    findDerivationsOrTargetsWithSuccessResult(premiseStatements).strip3.mapLeft(premiseStatements.zip(_).map { case (premiseStatement, derivation) =>
      derivation.withKnownStatement(premiseStatement)
    })
  }

  private def findDerivationsOrTargetsWithSuccessResult(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivedStatement], Seq[Step.TargetStep], Boolean) = {
    premiseStatements.foldLeft((Seq.empty[DerivedStatement], Seq.empty[Step.TargetStep], false)) { case ((derivationsSoFar, targetStepsSoFar, wasSuccessfulSoFar), premiseStatement) =>
      val (derivation, targetsForThisPremise, wasThisStatementSuccessful) = findDerivationOrTargetsWithSuccessResult(premiseStatement)
      (derivationsSoFar :+ derivation, targetStepsSoFar ++ targetsForThisPremise, wasSuccessfulSoFar || wasThisStatementSuccessful)
    }
  }

  private def findDerivationOrTargetsWithSuccessResult(
    premiseStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (DerivedStatement, Seq[Step.TargetStep], Boolean) = {
    val directly = DerivationFinder.findDerivationForUnwrappedStatement(premiseStatement).map((_, Nil, true))

    def byDeconstructing = for {
      (step, deconstructedStatements) <- deconstruct(premiseStatement)
      (innerDerivations, innerTargets, wasSuccessful) = findDerivationsOrTargetsWithSuccessResult(deconstructedStatements)
      if wasSuccessful
    } yield (DerivedStatement.Combined(innerDerivations, step), innerTargets, true)

    def asTarget = {
      val (rewrittenStatement, applyRewritesOption) = DerivationFinder.rewriteWithKnownValues(premiseStatement)
      val (deconstruction, deconstructedStatements) = splitTarget(rewrittenStatement)
      (applyRewritesOption.fold(deconstruction)(_(deconstruction)), deconstructedStatements.map(Step.TargetStep(_)), false)
    }

    directly orElse byDeconstructing getOrElse asTarget
  }

  private def splitTarget(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (DerivedStatement, Seq[Statement]) = {
    def default = (DerivedStatement.Known(targetStatement), Seq(targetStatement))

    if (ExpressionUtils.isTypeLikeStatement(targetStatement)) {
      default
    } else {
      deconstruct(targetStatement) match {
        case Some((deconstructionStep, innerTargets)) => splitTargets(innerTargets).mapLeft(DerivedStatement.Combined(_, deconstructionStep))
        case None => default
      }
    }
  }

  private def splitTargets(
    targetStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivedStatement], Seq[Statement]) = {
    targetStatements.map(splitTarget).split.mapRight(_.flatten)
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
