package net.prover.model.components

import net.prover.model.Substitutions

import scala.collection.immutable.Nil

case class StatementVariable(text: String) extends Statement with Variable {
  override def boundVariables = Set.empty
  override def requiredSubstitutions = Substitutions.Required(Seq(this), Nil)
  override def calculateSubstitutions(other: Component, substitutions: Substitutions, boundVariableCount: Int) = {
    other match {
      case otherStatement: Statement =>
        substitutions.addVariable(this, otherStatement).toSeq
      case _ =>
        Nil
    }
  }
  def applySubstitutions(substitutions: Substitutions): Option[Statement] = {
    substitutions.componentsByVariable.get(this).map(_.asInstanceOf[Statement])
  }
  override def replacePlaceholder(other: Component) = this
  override def calculateApplicatives(argument: Term, substitutions: Substitutions, boundVariableCount: Int) = {
    Seq((Predicate.Constant(this), substitutions))
  }

  override def toString: String = text
  override def serialized: String = text
}
