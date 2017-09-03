package net.prover.model.components
import net.prover.model.Substitutions

case class PredicateApplication(predicateName: String, argument: Term) extends Statement {
  override def requiredSubstitutions = Substitutions.Required(Nil, Seq(predicateName))
  override def calculateSubstitutions(other: Component, substitutions: Substitutions): Seq[Substitutions] = {
    other match {
      case PredicateApplication(otherName, otherArgument) =>
        argument.calculateSubstitutions(otherArgument, substitutions).flatMap(_.addPredicate(predicateName, Predicate.Named(otherName)))
      case statement: Statement =>
        statement.calculateApplicatives(argument, substitutions).flatMap { case (predicate, newSubstitutions) =>
          newSubstitutions.addPredicate(predicateName, predicate)
        }
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Option[Statement] = {
    for {
      predicate <- substitutions.predicatesByName.get(predicateName)
      updatedArgument <- argument.applySubstitutions(substitutions)
    } yield predicate(updatedArgument)
  }
  override def replacePlaceholder(other: Component) = this
  override def serialized: String = s"with ${argument.serialized} $predicateName"
}
