package net.prover.model.proof

case class StepContext private(stepReference: StepReference, boundVariableLists: Seq[Seq[String]]) {
  def externalDepth: Int = boundVariableLists.length
  def atIndex(index: Int): StepContext = copy(stepReference = stepReference.forChild(index))
  def addBoundVariable(name: String): StepContext = copy(boundVariableLists = boundVariableLists :+ Seq(name))
}
object StepContext {
  def empty: StepContext = StepContext(StepReference(Nil), Nil)
}

