package net.prover.model.proof

import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step.NewAssert
import net.prover.model.proof.Step.NewAssert.Premise.SingleLinePremise

object ProofHelper {
  private def getSimplification(premise: SingleLinePremise, simplificationInference: Inference, stepContext: StepContext): Option[SingleLinePremise] = {
    for {
      inferencePremise <- simplificationInference.premises.single
      substitutions <- inferencePremise.calculateSubstitutions(premise.statement, Substitutions.empty, 0, stepContext.externalDepth).headOption
      simplifiedTarget <- simplificationInference.conclusion.applySubstitutions(substitutions, 0, stepContext.externalDepth)
      path <- inferencePremise.findComponentPath(simplificationInference.conclusion)
    } yield {
      NewAssert.Premise.Simplification(simplifiedTarget, premise, simplificationInference.summary, substitutions, path)
    }
  }
  private def getSingleSimplifications(premise: SingleLinePremise, simplificationInferences: Seq[Inference], stepContext: StepContext): Seq[SingleLinePremise] = {
    simplificationInferences.mapCollect(i => getSimplification(premise, i, stepContext))
  }

  private def getSimplifications(premises: Seq[SingleLinePremise], simplificationInferences: Seq[Inference], stepContext: StepContext): Seq[SingleLinePremise] = {
    def helper(previous: Seq[SingleLinePremise], next: Seq[SingleLinePremise]): Seq[SingleLinePremise] = {
      if (next.isEmpty)
        previous
      else
        helper(previous ++ next, next.flatMap(getSingleSimplifications(_, simplificationInferences, stepContext)))
    }
    helper(Nil, premises)
  }

  def getAvailablePremises(stepContext: StepContext, parsingContext: ParsingContext): Seq[NewAssert.Premise.SingleLinePremise] = {
    val basePremises = stepContext.availableStatements.map { s=>
      NewAssert.Premise.Given(s.statement, s.reference)
    }
    val simplificationInferences = parsingContext.inferences.filter(_.rearrangementType == RearrangementType.Simplification)
    getSimplifications(basePremises, simplificationInferences, stepContext)
  }

  def findPremise(target: Statement, stepContext: StepContext, parsingContext: ParsingContext): NewAssert.Premise = {
    getAvailablePremises(stepContext, parsingContext).find(_.statement == target).getOrElse(NewAssert.Premise.Pending(target))
  }

  def findFact(target: Statement, stepContext: StepContext, parsingContext: ParsingContext): Option[NewAssert] = {
    parsingContext.inferences
      .filter(_.premises.isEmpty)
      .mapFind { inference =>
        inference.conclusion.calculateSubstitutions(target, Substitutions.empty, 0, stepContext.externalDepth).headOption
          .map { substitutions =>
            NewAssert(target, inference.summary, Nil, substitutions)
          }
      }
  }
}
