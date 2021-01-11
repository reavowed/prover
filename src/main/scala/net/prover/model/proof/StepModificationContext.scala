package net.prover.model.proof

import net.prover.core.transformers.ParameterInserter.ParameterInsertionContext
import net.prover.model.substitutions.ContextWithExternalDepth

case class StepModificationContext(externalDepth: Int) extends ContextWithExternalDepth {
  def increaseDepth: StepModificationContext = StepModificationContext(externalDepth + 1)
  def toParameterInsertionContext: ParameterInsertionContext = ParameterInsertionContext(internalDepth)
}
object StepModificationContext {
  def initial: StepModificationContext = StepModificationContext(0)
}
