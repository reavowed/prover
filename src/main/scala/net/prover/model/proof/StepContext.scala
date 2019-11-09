package net.prover.model.proof

import net.prover.model.EntryContext
import net.prover.model.expressions.Statement

case class StepContext private(
    stepReference: StepReference,
    termVariableNames: Seq[String],
    boundVariableLists: Seq[Seq[String]],
    premisesAndSimplifications: Seq[(Premise.Given, Seq[Premise.Simplification])],
    entryContext: EntryContext)
{
  def externalDepth: Int = boundVariableLists.length
  def atIndex(index: Int): StepContext = copy(stepReference = stepReference.forChild(index))
  def addBoundVariable(name: String): StepContext = copy(
    boundVariableLists = boundVariableLists :+ Seq(name),
    premisesAndSimplifications = premisesAndSimplifications.map(_.mapLeft(_.insertExternalParameters(1)).mapRight(_.map(_.insertExternalParameters(1)))))

  private def addPremise(givenPremise: Premise.Given): StepContext = {
    copy(premisesAndSimplifications = premisesAndSimplifications :+ (givenPremise, SimplificationFinder.getSimplifications(givenPremise, entryContext, this)))
  }
  private def addStatement(statement: Statement, reference: PreviousLineReference): StepContext = {
    addPremise(Premise.Given(statement, reference))
  }
  def addStatement(statement: Statement, suffix: String): StepContext = {
    addStatement(statement, stepReference.withSuffix(suffix))
  }
  def addStep(step: Step, reference: PreviousLineReference): StepContext = {
    step.provenStatement.map(addStatement(_, reference)).getOrElse(this)
  }
  def addSteps(steps: Seq[Step]): StepContext = {
    steps.zipWithIndex.foldLeft(this) { case (context, (step, index)) =>
      context.addStep(step, stepReference.forChild(index))
    }
  }

  def allPremisesSimplestFirst: Seq[Premise.SingleLinePremise] = {
    premisesAndSimplifications.flatMap { case (premise, simplifications) => simplifications.reverse :+ premise }
  }
  def allPremisesSimplestLast: Seq[Premise.SingleLinePremise] = {
    premisesAndSimplifications.flatMap { case (premise, simplifications) => premise +: simplifications }
  }

  def findPremise(statement: Statement): Option[Premise.SingleLinePremise] = {
    allPremisesSimplestLast.find(_.statement == statement)
  }
  def createPremise(statement: Statement): Premise = {
    findPremise(statement) getOrElse Premise.Pending(statement)
  }
}
object StepContext {
  def withPremisesAndTerms(premises: Seq[Statement], termVariableNames: Seq[String], entryContext: EntryContext): StepContext = {
    val emptyContext = StepContext(StepReference(Nil), termVariableNames, Nil, Nil, entryContext)
    premises.zipWithIndex.foldLeft(emptyContext) { case (context, (premise, index)) =>
      context.addStatement(premise, PremiseReference(index))
    }
  }
}

