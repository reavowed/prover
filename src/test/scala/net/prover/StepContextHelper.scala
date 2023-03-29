package net.prover

import net.prover.entries.{ProofWithContext, StepsWithContext, TypedStepWithContext}
import net.prover.model.TestDefinitions.{entryContextToProvingContext, mock, theStubbed}
import net.prover.model.{EntryContext, VariableDefinitions}
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepContext, StepReference}

import scala.reflect.ClassTag

trait StepContextHelper {

  val bookKey = "test-book-key"
  val chapterKey = "test-chapter-key"
  val theoremKey = "test-theorem-key"
  val proofIndex = 3
  val outerStepPath = Seq(3, 1, 4, 1)
  val stepIndex = 5
  val stepPath = outerStepPath :+ stepIndex

  def createBaseStepContext(premises: Seq[Statement])(implicit variableDefinitions: VariableDefinitions): StepContext = {
    StepContext.withPremisesAndVariables(premises, variableDefinitions)
  }

  def createBaseStepContext(premises: Seq[Statement], boundVariables: Seq[String])(implicit variableDefinitions: VariableDefinitions): StepContext = {
    val baseContext = createBaseStepContext(premises)
    boundVariables.foldLeft(baseContext) { case (context, variable) => context.addBoundVariable(variable) }
  }

  def createBaseStepContext(premises: Seq[Statement], depth: Int)(implicit variableDefinitions: VariableDefinitions): StepContext = {
    createBaseStepContext(premises, (0 until depth).map(i => s"x_$i"))
  }

  def createOuterStepContext(boundVariables: Seq[String])(implicit variableDefinitions: VariableDefinitions): StepContext = {
    val baseContext = createBaseStepContext(Nil, boundVariables)
    outerStepPath.foldLeft(baseContext) { case (context, index) => context.atIndex(index) }
  }

  def createTargetStepWithContext(
    statement: Statement)(
    implicit entryContext: EntryContext,
    stepContext: StepContext
  ): TypedStepWithContext[Step.Target] = {
    val stepsWithContext = mock[StepsWithContext]
    stepsWithContext.provingContext returns entryContextToProvingContext(entryContext)
    TypedStepWithContext(
      Step.Target(statement),
      stepIndex,
      stepContext,
      stepsWithContext)
  }

  def createStepsWithContext(
    steps: Seq[Step])(
    implicit entryContext: EntryContext,
    outerStepContext: StepContext
  ): StepsWithContext = {
    val proofWithContext = mock[ProofWithContext]
    proofWithContext.provingContext returns entryContextToProvingContext(entryContext)
    StepsWithContext(
      steps,
      outerStepContext.stepReference,
      outerStepContext,
      proofWithContext)
  }
}
