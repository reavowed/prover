package net.prover.model.components

import net.prover.model.Substitutions

import scala.collection.immutable.Nil

case class BoundVariable(level: Int)(val text: String) extends Term {
  override def boundVariables = Set(level)
  override def requiredSubstitutions = Substitutions.Required.empty
  override def serialized = text
  override def calculateSubstitutions(other: Component, substitutionsSoFar: Substitutions, boundVariableCount: Int) = {
    if (other == this) Seq(substitutionsSoFar) else Nil
  }
  override def applySubstitutions(substitutions: Substitutions): Option[BoundVariable] = Some(this)
  override def replacePlaceholder(other: Component) = this
  override def calculateApplicatives(argument: Term, substitutions: Substitutions, boundVariableCount: Int) = {
    if (argument == this) {
      Seq((Function.Identity, substitutions))
    } else {
      Seq((Function.Constant(this), substitutions))
    }
  }

  override def toString: String = text
}
