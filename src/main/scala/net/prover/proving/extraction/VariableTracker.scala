package net.prover.proving.extraction

import net.prover.model.Inference
import net.prover.model.proof.StepContext

case class VariableTracker(baseVariableNames: Seq[String], additionalVariableNames: Seq[String]) {
  def namesUsedSoFar: Seq[String] = baseVariableNames ++ additionalVariableNames
  def getAndAddUniqueVariableName(baseName: String): (String, Int, VariableTracker) = {
    val newName = if (!namesUsedSoFar.contains(baseName))
      baseName
    else {
      val i = LazyList.from(1).find(i => !namesUsedSoFar.contains(s"${baseName}_$i")).get
      s"${baseName}_$i"
    }
    (newName, baseVariableNames.length + additionalVariableNames.length, VariableTracker(baseVariableNames, additionalVariableNames :+ newName))
  }
}

object VariableTracker {
  def fromInference(inference: Inference): VariableTracker = VariableTracker(inference.variableDefinitions.terms.map(_.name), Nil)
  def fromStepContext(implicit stepContext: StepContext): VariableTracker = VariableTracker(stepContext.variableDefinitions.terms.map(_.name), Nil)
}
