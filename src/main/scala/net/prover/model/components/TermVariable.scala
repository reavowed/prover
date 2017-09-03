package net.prover.model.components

import net.prover.model.{Html, Substitutions}

import scala.collection.immutable.Nil

case class TermVariable(text: String) extends Term with Variable {
  override def requiredSubstitutions = Substitutions.Required(Seq(this), Nil)
  override def calculateSubstitutions(
    other: Component,
    substitutions: Substitutions
  ): Seq[Substitutions] = {
    other match {
      case otherTerm: Term =>
        substitutions.addVariable(this, otherTerm).toSeq
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Option[Term] = {
    substitutions.componentsByVariable.get(this).map(_.asInstanceOf[Term])
  }
  override def replacePlaceholder(other: Component) = this
  override def toString: String = text
  override def serialized: String = text
}
