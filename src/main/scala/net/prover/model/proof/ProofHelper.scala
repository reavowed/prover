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

  def getAvailablePremises(stepContext: StepContext, parsingContext: ParsingContext): Seq[Premise.SingleLinePremise] = {
    val basePremises = stepContext.availableStatements.map { s=>
      Premise.Given(s.statement, s.reference)
    }
    val simplificationInferences = parsingContext.inferences.filter(_.rearrangementType == RearrangementType.Simplification)
    getSimplifications(basePremises, simplificationInferences, stepContext)
  }

  def findPremise(target: Statement, stepContext: StepContext, parsingContext: ParsingContext): Premise = {
    getAvailablePremises(stepContext, parsingContext).find(_.statement == target).getOrElse(Premise.Pending(target))
  }

  def findFact(target: Statement, stepContext: StepContext, parsingContext: ParsingContext): Option[Step.Assertion] = {
    parsingContext.inferences
      .filter(_.premises.isEmpty)
      .mapFind { inference =>
        inference.conclusion.calculateSubstitutions(target, Substitutions.empty, 0, stepContext.externalDepth).headOption
          .map { substitutions =>
            Step.Assertion(target, inference.summary, Nil, substitutions)
          }
      }
  }

  def findNamingInferences(parsingContext: ParsingContext): Seq[(Inference, Seq[Statement], Statement)] = {
    parsingContext.inferences.mapCollect(i =>
      getNamingPremisesAndAssumption(i, parsingContext).map {
        case (premises, assumption) => (i, premises, assumption)
      })
  }

  def getNamingPremisesAndAssumption(inference: Inference, parsingContext: ParsingContext): Option[(Seq[Statement], Statement)] = {
    (parsingContext.scopingStatementOption, parsingContext.deductionStatementOption) match {
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
