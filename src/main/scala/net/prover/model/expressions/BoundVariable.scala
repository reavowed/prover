package net.prover.model.expressions

import net.prover.model.Substitutions

import scala.collection.immutable.Nil

case class BoundVariable(level: Int)(val text: String) extends Term {
  override def boundVariables = Set(level)
  override def requiredSubstitutions = Substitutions.Required.empty
  override def serialized = text
  override def calculateSubstitutions(other: Expression, substitutionsSoFar: Substitutions, boundVariableCount: Int) = {
    if (other == this) Seq(substitutionsSoFar) else Nil
  }
  override def applySubstitutions(substitutions: Substitutions): Option[BoundVariable] = Some(this)
  override def replacePlaceholder(other: Expression) = this
  override def calculateApplicatives(argument: Term, substitutions: Substitutions, boundVariableCount: Int) = {
    if (argument == this) {
      Seq((Function.Identity, substitutions))
    } else if (level >= boundVariableCount) {
      super.calculateApplicatives(argument, substitutions, boundVariableCount) :+ (Function.Constant(this), substitutions)
    } else {
      Seq((Function.Constant(this), substitutions))
    }
  }

  override def toString: String = text
}
