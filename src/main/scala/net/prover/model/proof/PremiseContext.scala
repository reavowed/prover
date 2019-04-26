package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions.Statement

case class PremiseContext(
  premisesAndSimplifications: Seq[(Premise.Given, Seq[Premise.Simplification])],
  entryContext: EntryContext)
{
  private def addPremise(givenPremise: Premise.Given, externalDepth: Int): PremiseContext = {
    copy(premisesAndSimplifications = premisesAndSimplifications :+ (givenPremise, ProofHelper.getSimplifications(givenPremise, entryContext, externalDepth)))
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

  def allPremises: Seq[Premise.SingleLinePremise] = premisesAndSimplifications.map(_._1) ++ premisesAndSimplifications.flatMap(_._2)

  def findPremise(statement: Statement): Option[Premise.SingleLinePremise] = {
    allPremises.find(_.statement == statement)
  }
  def createPremise(statement: Statement): Premise = {
    findPremise(statement) getOrElse Premise.Pending(statement)
  }

  def addBoundVariable(): PremiseContext = {
    PremiseContext(
      premisesAndSimplifications = premisesAndSimplifications.map(_.mapLeft(_.insertExternalParameters(1)).mapRight(_.map(_.insertExternalParameters(1)))),
      entryContext)
  }
}

object PremiseContext {
  def justWithPremises(premises: Seq[Statement], entryContext: EntryContext): PremiseContext = {
    val emptyContext = PremiseContext(Nil, entryContext)
    premises.zipWithIndex.foldLeft(emptyContext) { case (context, (premise, index)) =>
      context.addStatement(premise, PremiseReference(index), 0)
    }
  }
}
