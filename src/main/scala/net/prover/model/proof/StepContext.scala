package net.prover.model.proof

import net.prover.model.VariableDefinitions
import net.prover.core.expressions.Statement

case class StepContext private(
    stepReference: StepReference,
    variableDefinitions: VariableDefinitions,
    boundVariableLists: Seq[Seq[String]],
    premises: Seq[Premise.Given])
  extends SubstitutionContext
{
  def externalDepth: Int = boundVariableLists.length
  def atIndex(index: Int): StepContext = copy(stepReference = stepReference.forChild(index))
  def addBoundVariable(name: String): StepContext = copy(
    boundVariableLists = boundVariableLists :+ Seq(name),
    premises = premises.map(StepParameterInserter.insertExternalParameters(_, 1).asInstanceOf[Premise.Given]))

  private def addPremise(givenPremise: Premise.Given): StepContext = {
    copy(premises = premises :+ givenPremise)
  }
  def addStatement(statement: Statement, reference: PreviousLineReference): StepContext = {
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
  def withPremisesAndVariables(premises: Seq[Statement], variableDefinitions: VariableDefinitions): StepContext = {
    val emptyContext = StepContext(StepReference(Nil), variableDefinitions, Nil, Nil)
    premises.zipWithIndex.foldLeft(emptyContext) { case (context, (premise, index)) =>
      context.addStatement(premise, PremiseReference(index))
    }
  }
  implicit def fromStepProvingContext(implicit stepProvingContext: StepProvingContext): StepContext = stepProvingContext.stepContext
}

