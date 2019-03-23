package net.prover.model.proof

import net.prover.model.Inference.RearrangementType
import net.prover.model.expressions.Statement
import net.prover.model._
import net.prover.model.proof.Step.NewAssert
import net.prover.model.proof.Step.NewAssert.Premise.SingleLinePremise

object ProofHelper {
  private def getSimplification(premise: SingleLinePremise, simplificationInference: Inference, stepContext: StepContext): Option[SingleLinePremise] = {
    for {
      inferencePremise <- simplificationInference.premises.single
      substitutions <- inferencePremise.statement.calculateSubstitutions(premise.statement, Substitutions.empty, 0, stepContext.externalDepth).headOption
      simplifiedTarget <- simplificationInference.conclusion.applySubstitutions(substitutions, 0, stepContext.externalDepth)
      path <- inferencePremise.statement.findComponentPath(simplificationInference.conclusion)
    } yield {
      NewAssert.Premise.Simplification(simplifiedTarget, premise, simplificationInference.summary, substitutions, path)
    }
  }
  private def getSingleSimplifications(premise: SingleLinePremise, simplificationInferences: Seq[Inference], stepContext: StepContext): Seq[SingleLinePremise] = {
    simplificationInferences.mapCollect(i => getSimplification(premise, i, stepContext))
  }

  def getSimplifications(premises: Seq[SingleLinePremise], simplificationInferences: Seq[Inference], stepContext: StepContext): Seq[SingleLinePremise] = {
    def helper(previous: Seq[SingleLinePremise], next: Seq[SingleLinePremise]): Seq[SingleLinePremise] = {
      if (next.isEmpty)
        previous
      else
        helper(previous ++ next, next.flatMap(getSingleSimplifications(_, simplificationInferences, stepContext)))
    }
    helper(Nil, premises)
  }

  def findPremise(target: Statement, stepContext: StepContext, parsingContext: ParsingContext): Option[NewAssert.Premise.SingleLinePremise] = {
    val basePremises = stepContext.availableStatements.map { s=>
      NewAssert.Premise.Given(s.statement, s.reference)
    }
    val simplificationInferences = parsingContext.inferences.filter(_.rearrangementType == RearrangementType.Simplification)
    val allPremises = getSimplifications(basePremises, simplificationInferences, stepContext)
    allPremises.find(_.statement == target)
  }
}
