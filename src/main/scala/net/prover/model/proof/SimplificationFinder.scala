package net.prover.model.proof

import net.prover.model.{EntryContext, Inference}

object SimplificationFinder {
  private def getSimplification(premise: Premise.SingleLinePremise, simplificationInference: Inference)(implicit stepContext: StepContext): Option[Premise.Simplification] = {
    for {
      inferencePremise <- simplificationInference.premises.single
      substitutions <- inferencePremise.calculateSubstitutions(premise.statement).flatMap(_.confirmTotality)
      simplifiedTarget <- simplificationInference.conclusion.applySubstitutions(substitutions)
      path <- inferencePremise.findComponentPath(simplificationInference.conclusion)
    } yield {
      Premise.Simplification(simplifiedTarget, premise, simplificationInference.summary, substitutions, path)
    }
  }
  private def getSingleSimplifications(premise: Premise.SingleLinePremise, simplificationInferences: Seq[Inference])(implicit stepContext: StepContext): Seq[Premise.Simplification] = {
    simplificationInferences.mapCollect(i => getSimplification(premise, i))
  }
  def getSimplifications(premise: Premise.Given, entryContext: EntryContext)(implicit stepContext: StepContext): Seq[Premise.Simplification] = {
    @scala.annotation.tailrec
    def helper(acc: Seq[Premise.Simplification], toCalculate: Seq[Premise.SingleLinePremise]): Seq[Premise.Simplification] = {
      if (toCalculate.isEmpty)
        acc
      else {
        val next = toCalculate.flatMap(getSingleSimplifications(_, entryContext.simplificationInferences))
        helper(acc ++ next, next)
      }
    }
    helper(Nil, Seq(premise))
  }
}
