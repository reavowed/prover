package net.prover.model.components

import net.prover.model.Substitutions

import scala.collection.immutable.Nil

case class StatementVariable(text: String) extends Statement with Variable {
  override def variables: Set[Variable] = Set(this)
  override def boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = (Set.empty, Set.empty)
  override def calculateSubstitutions(
    other: Component,
    substitutions: Substitutions
  ): Seq[Substitutions] = other match {
    case otherStatement: Statement =>
      substitutions.tryAdd(this, otherStatement).toSeq
    case _ =>
      Nil
  }
  def applySubstitutions(substitutions: Substitutions): Option[Statement] = {
    substitutions.componentsByVariable.get(this).map(_.asInstanceOf[Statement])
  }
  override def replacePlaceholder(other: Component) = Some(this)

  override def toString: String = text
  override def html: String = text
  override def serialized: String = text
}
