package net.prover.model.components
import net.prover.model.Substitutions

case class Predicate(text: String, argument: TermVariable) extends Statement with Variable {
  override def variablesRequiringSubstitution: Seq[Variable] = Seq(this)
  override def calculateSubstitutions(other: Component, substitutions: Substitutions): Seq[Substitutions] = ???
  override def applySubstitutions(substitutions: Substitutions): Option[Statement] = ???
  override def replacePlaceholder(other: Component): Option[Statement] = Some(this)
  override def serialized: String = s"with ${argument.text} $text"
}
