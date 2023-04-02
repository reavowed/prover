package net.prover.model.proof

import net.prover.model.expressions.Statement
import net.prover.model.{Inference, ProvingContext, Substitutions}

object SimplificationFinder {
  private def getSimplification(statement: Statement, simplificationInference: Inference, inferencePremise: Statement)(implicit substitutionContext: SubstitutionContext): Option[(Statement, Substitutions)] = {
    for {
      substitutions <- inferencePremise.calculateSubstitutions(statement).flatMap(_.confirmTotality(simplificationInference.variableDefinitions))
      simplifiedTarget <- simplificationInference.conclusion.applySubstitutions(substitutions)
    } yield {
      (simplifiedTarget, substitutions)
    }
  }
  private def getSimplification(premise: Premise.SingleLinePremise, simplificationInference: Inference, inferencePremise: Statement)(implicit substitutionContext: SubstitutionContext): Option[Premise.Simplification] = {
    for {
      (simplifiedTarget, substitutions) <- getSimplification(premise.statement, simplificationInference, inferencePremise)
      path <- inferencePremise.findComponentPath(simplificationInference.conclusion)
    } yield {
      Premise.Simplification(simplifiedTarget, premise, simplificationInference.summary, substitutions, path)
    }
  }

  private def getSimplifications[T, S <: T](t: T, getSimplification: (T, Inference, Statement) => Option[S])(implicit provingContext: ProvingContext): Seq[S] = {
    def helper(acc: Seq[S], toCalculate: Seq[T]): Seq[S] = {
      if (toCalculate.isEmpty)
        acc
      else {
        val next = for {
          t <- toCalculate
          (inference, inferencePremise) <- provingContext.structuralSimplificationInferences
          simplification <- getSimplification(t, inference, inferencePremise)
        } yield simplification
        helper(acc ++ next, next)
      }
    }
    helper(Nil, Seq(t))
  }

  def getSimplifications(statement: Statement)(implicit substitutionContext: SubstitutionContext, provingContext: ProvingContext): Seq[Statement] = {
    getSimplifications[Statement, Statement](statement, getSimplification(_, _, _).map(_._1))
  }
  def getSimplifications(premise: Premise.Given)(implicit provingContext: ProvingContext, substitutionContext: SubstitutionContext): Seq[Premise.Simplification] = {
    getSimplifications(premise, getSimplification)
  }

}
