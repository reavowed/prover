package net.prover.model.proof

import net.prover.model.ProvingContext
import net.prover.model.expressions.Statement

case class StepContext private(
    stepReference: StepReference,
    termVariableNames: Seq[String],
    boundVariableLists: Seq[Seq[String]],
    premises: Seq[Premise.Given])
  extends SubstitutionContext
{
  def externalDepth: Int = boundVariableLists.length
  def atIndex(index: Int): StepContext = copy(stepReference = stepReference.forChild(index))
  def addBoundVariable(name: String): StepContext = copy(
    boundVariableLists = boundVariableLists :+ Seq(name),
    premises = premises.map(_.insertExternalParameters(1, 0)))

  private def addPremise(givenPremise: Premise.Given): StepContext = {
    copy(premises = premises :+ givenPremise)
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
}
object StepContext {
  def withPremisesAndTerms(premises: Seq[Statement], termVariableNames: Seq[String]): StepContext = {
    val emptyContext = StepContext(StepReference(Nil), termVariableNames, Nil, Nil)
    premises.zipWithIndex.foldLeft(emptyContext) { case (context, (premise, index)) =>
      context.addStatement(premise, PremiseReference(index))
    }
  }
  implicit def fromStepProvingContext(implicit stepProvingContext: StepProvingContext): StepContext = stepProvingContext.stepContext
}

