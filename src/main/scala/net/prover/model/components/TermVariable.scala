package net.prover.model.components

import net.prover.model.{Html, Substitutions}

import scala.collection.immutable.Nil

case class TermVariable(text: String) extends Term with Variable {
  override def boundVariables = Set.empty
  override def requiredSubstitutions = Substitutions.Required(Seq(this), Nil)
  override def calculateSubstitutions(other: Component, substitutions: Substitutions, boundVariableCount: Int) = {
    other match {
      case otherTerm: Term if otherTerm.boundVariables.forall(_ >= boundVariableCount) =>
        substitutions.addVariable(this, otherTerm).toSeq
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Option[Term] = {
    substitutions.componentsByVariable.get(this).map(_.asInstanceOf[Term])
  }
  override def replacePlaceholder(other: Component) = this
  override def calculateApplicatives(argument: Term, substitutions: Substitutions, boundVariableCount: Int) = {
    super.calculateApplicatives(argument, substitutions, boundVariableCount) :+ (Function.Constant(this), substitutions)
  }
  override def toString: String = text
  override def serialized: String = text
}
