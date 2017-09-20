package net.prover.model.expressions
import net.prover.model.Substitutions

case class PredicateApplication(predicateName: String, argument: Term) extends Statement {
  override def boundVariables = argument.boundVariables
  override def requiredSubstitutions = argument.requiredSubstitutions ++ Substitutions.Required(Nil, Seq(predicateName))
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions, boundVariableCount: Int) = {
    other match {
      case PredicateApplication(otherName, otherArgument) =>
        argument
          .calculateSubstitutions(otherArgument, substitutions, boundVariableCount)
          .flatMap(_.addPredicate(predicateName, PredicateVariable(otherName)))
      case statement: Statement =>
        statement
          .calculateApplicatives(argument, substitutions, boundVariableCount)
          .flatMap { case (predicate, newSubstitutions) =>
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
  override def replacePlaceholder(other: Expression) = this
  override def calculateApplicatives(targetArgument: Term, substitutions: Substitutions, boundVariableCount: Int) = {
    targetArgument.calculateSubstitutions(argument, substitutions, boundVariableCount).map(PredicateVariable(predicateName) -> _)
  }
  override def makeApplicative(argument: Term) = None

  override def toString: String = s"$predicateName($argument)"
  override def serialized: String = s"with ${argument.serialized} $predicateName"
}
