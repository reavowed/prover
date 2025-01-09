package net.prover.model.proof

import net.prover.entries.StepWithContext
import net.prover.model.VariableDefinitions
import net.prover.model.expressions.Statement

case class StepContext private(
    parentReference: StepReference,
    index: Int,
    variableDefinitions: VariableDefinitions,
    boundVariableLists: Seq[Seq[String]],
    premises: Seq[Premise.Given])
  extends SubstitutionContext
{
  lazy val stepReference: StepReference = parentReference.forChild(index)

  def forChild(): StepContext = copy(parentReference = parentReference.forChild(index), index = 0)
  def externalDepth: Int = boundVariableLists.length
  def addBoundVariable(name: String): StepContext = copy(
    boundVariableLists = boundVariableLists :+ Seq(name),
    premises = premises.map(_.insertExternalParameters(1, 0)))

  private def addPremise(givenPremise: Premise.Given): StepContext = {
    copy(premises = premises :+ givenPremise)
  }
  def addPremise(statement: Statement, reference: PreviousLineReference): StepContext = {
    addPremise(Premise.Given(statement, reference))
  }

  def addAssumption(assumption: Statement): StepContext = addPremise(assumption, stepReference.withSuffix("a"))

  def addStep(step: Step): StepContext = {
    addPremise(step.statement, stepReference)
      .copy(index = index + 1)
  }
  def addSteps(steps: Seq[Step]): StepContext = {
    steps.foldLeft(this)(_.addStep(_))
  }
}

trait LowPriorityStepContextImplicits {
  implicit def fromStepProvingContext(implicit stepProvingContext: StepProvingContext): StepContext = stepProvingContext.stepContext
}
object StepContext extends LowPriorityStepContextImplicits {
  def withPremisesAndVariables(
    premises: Seq[Statement],
    variableDefinitions: VariableDefinitions
  ): StepContext = {
    val emptyContext = StepContext(StepReference(Nil), 0, variableDefinitions, Nil, Nil)
    premises.zipWithIndex.foldLeft(emptyContext) { case (context, (premise, index)) =>
      context.addPremise(premise, PremiseReference(index))
    }
  }

  implicit def fromStepWithContext(stepWithContext: StepWithContext): StepContext = stepWithContext.stepContext
  implicit def implicitlyFromStepWithContext(implicit stepWithContext: StepWithContext): StepContext = fromStepWithContext(stepWithContext)
}

