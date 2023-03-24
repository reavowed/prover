package net.prover

import net.prover.model.VariableDefinitions
import net.prover.model.expressions.Statement
import net.prover.model.proof.StepContext

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
}
