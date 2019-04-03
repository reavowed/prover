package net.prover.model.proof

import net.prover.model.EntryContext
import net.prover.model.expressions.Statement

case class PremiseContext(
  givenPremises: Seq[Premise.Given],
  simplifiedPremises: Seq[Premise.Simplification],
  entryContext: EntryContext)
{
  private def addPremise(givenPremise: Premise.Given, externalDepth: Int): PremiseContext = {
    copy(
      givenPremises = givenPremises :+ givenPremise,
      simplifiedPremises = simplifiedPremises ++ ProofHelper.getSimplifications(givenPremise, entryContext, externalDepth))
  }
  private def addStatement(statement: Statement, reference: PreviousLineReference, externalDepth: Int): PremiseContext = {
    addPremise(Premise.Given(statement, reference), externalDepth)
  }
  private def addStatement(statement: Statement, stepContext: StepContext): PremiseContext = {
    addStatement(statement, stepContext.stepReference, stepContext.externalDepth)
  }
  def addStatement(statement: Statement, suffix: String, stepContext: StepContext): PremiseContext = {
    addStatement(statement, stepContext.stepReference.withSuffix(suffix), stepContext.externalDepth)
  }
  def addStep(step: Step, stepContext: StepContext): PremiseContext = {
    step.provenStatement.map(addStatement(_, stepContext)).getOrElse(this)
  }
  def addSteps(steps: Seq[Step], stepContext: StepContext): PremiseContext = {
    steps.zipWithIndex.foldLeft(this) { case (context, (step, index)) =>
      context.addStep(step, stepContext.atIndex(index))
    }
  }

  def findPremise(statement: Statement): Option[Premise.SingleLinePremise] = {
    givenPremises.find(_.statement == statement) orElse simplifiedPremises.find(_.statement == statement)
  }
  def createPremise(statement: Statement): Premise = {
    findPremise(statement) getOrElse Premise.Pending(statement)
  }

  def addBoundVariable(): PremiseContext = {
    PremiseContext(
      givenPremises = givenPremises.map(_.insertExternalParameters(1)),
      simplifiedPremises = simplifiedPremises.map(_.insertExternalParameters(1)),
      entryContext)
  }
}

object PremiseContext {
  def justWithPremises(premises: Seq[Statement], entryContext: EntryContext): PremiseContext = {
    val emptyContext = PremiseContext(Nil, Nil, entryContext)
    premises.zipWithIndex.foldLeft(emptyContext) { case (context, (premise, index)) =>
      context.addStatement(premise, PremiseReference(index), 0)
    }
  }
}
