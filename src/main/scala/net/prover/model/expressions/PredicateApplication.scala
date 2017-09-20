package net.prover.model.expressions
import net.prover.model.Substitutions

case class PredicateApplication(variable: PredicateVariable, argument: Term) extends Statement {
  override def boundVariables = argument.boundVariables
  override def requiredSubstitutions = argument.requiredSubstitutions ++ Substitutions.Required(Nil, Seq(variable))
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions, boundVariableCount: Int) = {
    other match {
      case PredicateApplication(otherVariable, otherArgument) =>
        argument
          .calculateSubstitutions(otherArgument, substitutions, boundVariableCount)
          .flatMap(_.addPredicate(variable, otherVariable))
      case statement: Statement =>
        statement
          .calculateApplicatives(argument, substitutions, boundVariableCount)
          .flatMap { case (predicate, newSubstitutions) =>
            newSubstitutions.addPredicate(variable, predicate)
          }
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Option[Statement] = {
    for {
      predicate <- substitutions.predicatesByName.get(variable)
      updatedArgument <- argument.applySubstitutions(substitutions)
    } yield predicate(updatedArgument)
  }
  override def replacePlaceholder(other: Expression) = this
  override def calculateApplicatives(targetArgument: Term, substitutions: Substitutions, boundVariableCount: Int) = {
    targetArgument.calculateSubstitutions(argument, substitutions, boundVariableCount).map(variable -> _)
  }
  override def makeApplicative(argument: Term) = None

  override def toString: String = s"$variable($argument)"
  override def serialized: String = s"with ${argument.serialized} $variable"
}
