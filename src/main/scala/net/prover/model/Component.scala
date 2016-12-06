package net.prover.model

trait Component[T <: Component[T]] {
  def variables: Variables
  def freeVariables: Seq[TermVariable]
  def attemptMatch(other: T): Option[MatchWithSubstitutions]
  def applyMatch(m: Match): T
  def substituteTermVariables(termToReplaceWith: TermVariable, termToBeReplaced: TermVariable): T
  def html: String
  override def toString: String = html
}

trait ComponentType[T <: Component[T]] {
  def parse(line: PartialLine, context: Context): (T, PartialLine)
}
