package net.prover.model.components

import net.prover.model.Substitutions

import scala.collection.immutable.Nil

case class BoundVariable(level: Int) extends Term {
  override def requiredSubstitutions = Substitutions.Required.empty
  override def serialized = s"$$bound-$level"
  override def calculateSubstitutions(other: Component, substitutionsSoFar: Substitutions): Seq[Substitutions] = {
    if (other == this) Seq(substitutionsSoFar) else Nil
  }
  override def applySubstitutions(substitutions: Substitutions): Option[BoundVariable] = Some(this)
  override def replacePlaceholder(other: Component) = this
}
