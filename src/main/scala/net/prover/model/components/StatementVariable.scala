package net.prover.model.components

import net.prover.model.Substitutions

import scala.collection.immutable.Nil

case class StatementVariable(text: String) extends Statement with Variable {
  override def requiredSubstitutions = Substitutions.Required(Seq(this), Nil)
  override def calculateSubstitutions(
    other: Component,
    substitutions: Substitutions
  ): Seq[Substitutions] = other match {
    case otherStatement: Statement =>
      substitutions.addVariable(this, otherStatement).toSeq
    case _ =>
      Nil
  }
  def applySubstitutions(substitutions: Substitutions): Option[Statement] = {
    substitutions.componentsByVariable.get(this).map(_.asInstanceOf[Statement])
  }
  override def replacePlaceholder(other: Component) = this

  override def toString: String = text
  override def serialized: String = text
}
