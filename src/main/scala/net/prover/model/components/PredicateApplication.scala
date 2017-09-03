package net.prover.model.components
import net.prover.model.Substitutions

case class PredicateApplication(text: String, argument: Term) extends Statement with Variable {
  override def requiredSubstitutions = Substitutions.Required(Nil, Seq(text))
  override def calculateSubstitutions(other: Component, substitutions: Substitutions): Seq[Substitutions] = {
    other match {
      case statement: Statement =>
        statement.calculateApplicatives(argument, substitutions).flatMap { case (predicate, newSubstitutions) =>
          newSubstitutions.addPredicate(text, predicate)
        }
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Option[Statement] = {
    for {
      predicate <- substitutions.predicatesByName.get(text)
      updatedArgument <- argument.applySubstitutions(substitutions)
    } yield predicate(updatedArgument)
  }
  override def replacePlaceholder(other: Component) = this
  override def serialized: String = s"with ${argument.serialized} $text"
}
