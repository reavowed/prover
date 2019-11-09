package net.prover.model.proof

import net.prover.model.{EntryContext, Inference}

object SimplificationFinder {
  private def getSimplification(premise: Premise.SingleLinePremise, simplificationInference: Inference, stepContext: StepContext): Option[Premise.Simplification] = {
    for {
      inferencePremise <- simplificationInference.premises.single
      substitutions <- inferencePremise.calculateSubstitutions(premise.statement, stepContext).headOption
      simplifiedTarget <- simplificationInference.conclusion.applySubstitutions(substitutions, stepContext)
      path <- inferencePremise.findComponentPath(simplificationInference.conclusion)
    } yield {
      Premise.Simplification(simplifiedTarget, premise, simplificationInference.summary, substitutions, path)
    }
  }
  private def getSingleSimplifications(premise: Premise.SingleLinePremise, simplificationInferences: Seq[Inference], stepContext: StepContext): Seq[Premise.Simplification] = {
    simplificationInferences.mapCollect(i => getSimplification(premise, i, stepContext))
  }
  def getSimplifications(premise: Premise.Given, entryContext: EntryContext, stepContext: StepContext): Seq[Premise.Simplification] = {
    def helper(acc: Seq[Premise.Simplification], toCalculate: Seq[Premise.SingleLinePremise]): Seq[Premise.Simplification] = {
      if (toCalculate.isEmpty)
        acc
      else {
        val next = toCalculate.flatMap(getSingleSimplifications(_, entryContext.simplificationInferences, stepContext))
        helper(acc ++ next, next)
      }
    }
    helper(Nil, Seq(premise))
  }
}
