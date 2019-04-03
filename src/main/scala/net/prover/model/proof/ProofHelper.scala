package net.prover.model.proof

import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.expressions.{DefinedStatement, Statement, StatementVariable}

object ProofHelper {
  private def getSimplification(premise: Premise.SingleLinePremise, simplificationInference: Inference, stepContext: StepContext): Option[Premise.SingleLinePremise] = {
    for {
      inferencePremise <- simplificationInference.premises.single
      substitutions <- inferencePremise.calculateSubstitutions(premise.statement, Substitutions.empty, 0, stepContext.externalDepth).headOption
      simplifiedTarget <- simplificationInference.conclusion.applySubstitutions(substitutions, 0, stepContext.externalDepth)
      path <- inferencePremise.findComponentPath(simplificationInference.conclusion)
    } yield {
      Premise.Simplification(simplifiedTarget, premise, simplificationInference.summary, substitutions, path)
    }
  }
  private def getSingleSimplifications(premise: Premise.SingleLinePremise, simplificationInferences: Seq[Inference], stepContext: StepContext): Seq[Premise.SingleLinePremise] = {
    simplificationInferences.mapCollect(i => getSimplification(premise, i, stepContext))
  }

  private def getSimplifications(premises: Seq[Premise.SingleLinePremise], simplificationInferences: Seq[Inference], stepContext: StepContext): Seq[Premise.SingleLinePremise] = {
    def helper(previous: Seq[Premise.SingleLinePremise], next: Seq[Premise.SingleLinePremise]): Seq[Premise.SingleLinePremise] = {
      if (next.isEmpty)
        previous
      else
        helper(previous ++ next, next.flatMap(getSingleSimplifications(_, simplificationInferences, stepContext)))
    }
    helper(Nil, premises)
  }

  def getAvailablePremises(stepContext: StepContext, entryContext: EntryContext): Seq[Premise.SingleLinePremise] = {
    val basePremises = stepContext.availableStatements.map { s=>
      Premise.Given(s.statement, s.reference)
    }
    val simplificationInferences = entryContext.inferences.filter(_.rearrangementType == RearrangementType.Simplification)
    getSimplifications(basePremises, simplificationInferences, stepContext)
  }

  def findPremise(target: Statement, stepContext: StepContext, entryContext: EntryContext): Premise = {
    getAvailablePremises(stepContext, entryContext).find(_.statement == target).getOrElse(Premise.Pending(target))
  }

  def findFact(target: Statement, stepContext: StepContext, entryContext: EntryContext): Option[Step.Assertion] = {
    entryContext.inferences
      .filter(_.premises.isEmpty)
      .mapFind { inference =>
        inference.conclusion.calculateSubstitutions(target, Substitutions.empty, 0, stepContext.externalDepth).headOption
          .map { substitutions =>
            Step.Assertion(target, inference.summary, Nil, substitutions)
          }
      }
  }

  def findNamingInferences(entryContext: EntryContext): Seq[(Inference, Seq[Statement], Statement)] = {
    entryContext.inferences.mapCollect(i =>
      getNamingPremisesAndAssumption(i, entryContext).map {
        case (premises, assumption) => (i, premises, assumption)
      })
  }

  def getNamingPremisesAndAssumption(inference: Inference, entryContext: EntryContext): Option[(Seq[Statement], Statement)] = {
    (entryContext.scopingStatementOption, entryContext.deductionStatementOption) match {
      case (Some(scopingStatement), Some(deductionStatement)) =>
        inference match {
          case Inference(
            _,
            initialPremises :+
              DefinedStatement(
              Seq(DefinedStatement(
              Seq(assumption: Statement, StatementVariable(deductionConclusionVariableName)),
              `deductionStatement`
              )),
              `scopingStatement`),
            StatementVariable(conclusionVariableName)
          ) if deductionConclusionVariableName == conclusionVariableName =>
            Some((initialPremises, assumption))
          case _ =>
            None
        }
      case _ =>
        None
    }
  }
}
