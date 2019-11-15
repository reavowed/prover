package net.prover.model.proof

import net.prover.model.Inference
import net.prover.model.expressions.Statement

object SimplificationFinder {
  private def getSimplification(premise: Premise.SingleLinePremise, simplificationInference: Inference, inferencePremise: Statement)(implicit stepContext: StepContext): Option[Premise.Simplification] = {
    for {
      substitutions <- inferencePremise.calculateSubstitutions(premise.statement).flatMap(_.confirmTotality)
      simplifiedTarget <- simplificationInference.conclusion.applySubstitutions(substitutions)
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
}
