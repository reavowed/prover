package net.prover.model.proof

import net.prover.model.{Inference, ProvingContext}
import net.prover.model.expressions.Statement
import net.prover.extensions.ExpressionExtensions._
import net.prover.substitutionFinding.transformers.PossibleSubstitutionCalculator

object SimplificationFinder {
  private def getSimplification(premise: Premise.SingleLinePremise, simplificationInference: Inference, inferencePremise: Statement)(implicit stepContext: StepContext): Option[Premise.Simplification] = {
    for {
      substitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(inferencePremise, premise.statement).flatMap(_.confirmTotality(simplificationInference.variableDefinitions))
      simplifiedTarget <- simplificationInference.conclusion.applySubstitutions(substitutions).toOption
      path <- inferencePremise.findComponentPath(simplificationInference.conclusion)
    } yield {
      Premise.Simplification(simplifiedTarget, premise, simplificationInference.summary, substitutions, path)
    }
  }
  def getSimplifications(premise: Premise.Given)(implicit stepProvingContext: StepProvingContext): Seq[Premise.Simplification] = {
    @scala.annotation.tailrec
    def helper(acc: Seq[Premise.Simplification], toCalculate: Seq[Premise.SingleLinePremise]): Seq[Premise.Simplification] = {
      if (toCalculate.isEmpty)
        acc
      else {
        val next = for {
          premise <- toCalculate
          (inference, inferencePremise) <- stepProvingContext.provingContext.structuralSimplificationInferences
          simplification <- getSimplification(premise, inference, inferencePremise)
        } yield simplification
        helper(acc ++ next, next)
      }
    }
    helper(Nil, Seq(premise))
  }

  private def getSimplification(statement: Statement, simplificationInference: Inference, inferencePremise: Statement)(implicit substitutionContext: SubstitutionContext): Option[Statement] = {
    for {
      substitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(inferencePremise, statement).flatMap(_.confirmTotality(simplificationInference.variableDefinitions))
      simplifiedTarget <- simplificationInference.conclusion.applySubstitutions(substitutions).toOption
    } yield {
      simplifiedTarget
    }
  }
  def getSimplifications(statement: Statement)(implicit substitutionContext: SubstitutionContext, provingContext: ProvingContext): Seq[Statement] = {
    def helper(acc: Seq[Statement], toCalculate: Seq[Statement]): Seq[Statement] = {
      if (toCalculate.isEmpty)
        acc
      else {
        val next = for {
          s <- toCalculate
          (inference, inferencePremise) <- provingContext.structuralSimplificationInferences
          simplification <- getSimplification(s, inference, inferencePremise)
        } yield simplification
        helper(acc ++ next, next)
      }
    }
    helper(Nil, Seq(statement))
  }
}
