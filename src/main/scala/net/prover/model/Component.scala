package net.prover.model

trait Component {
  def variables: Variables
  def freeVariables: Seq[TermVariable]
  def calculateSubstitutions(other: Component): Option[Substitutions]
  def applySubstitutions(substitutions: Substitutions): Component
  def substituteFreeVariable(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): Component
  def attemptSimplification(other: Component): Option[DistinctVariables]
  def makeSimplifications(distinctVariables: DistinctVariables): Component
  def html: String
  def safeHtml: String = html
  override def toString: String = html
}

trait ComponentType {
  def parse(line: PartialLine, context: Context): (Component, PartialLine)
}

object ComponentType {
  def parse(line: PartialLine): (ComponentType, PartialLine) = {
    line match {
      case WordAndRemainingText("term", lineAfterTerm) =>
        (Term, lineAfterTerm)
      case WordAndRemainingText("statement", lineAfterStatement) =>
        (Statement, lineAfterStatement)
      case WordAndRemainingText(something, _) =>
        throw ParseException.withMessage("Unrecognised statement type '$something'", line.fullLine)
    }
  }
}
