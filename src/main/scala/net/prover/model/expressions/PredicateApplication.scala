package net.prover.model.expressions
import net.prover.model.Substitutions

case class PredicateApplication(variable: PredicateVariable, arguments: Seq[Term]) extends Statement {
  override def boundVariables = arguments.boundVariables
  override def requiredSubstitutions = arguments.requiredSubstitutions ++ Substitutions.Required(Nil, Seq(variable))
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions, boundVariableCount: Int) = {
    other match {
      case PredicateApplication(otherVariable, otherArguments) =>
        arguments
          .calculateSubstitutions(otherArguments, substitutions, boundVariableCount)
          .flatMap(_.addPredicate(variable, otherVariable))
      case statement: Statement =>
        statement
          .calculateApplicatives(arguments, substitutions, boundVariableCount)
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
      updatedArguments <- arguments.applySubstitutions(substitutions).map(_.map(_.asInstanceOf[Term]))
    } yield predicate(updatedArguments)
  }
  override def replacePlaceholder(other: Expression) = this
  override def calculateApplicatives(targetArguments: Seq[Term], substitutions: Substitutions, boundVariableCount: Int) = {
    targetArguments.calculateSubstitutions(arguments, substitutions, boundVariableCount).map(variable -> _)
  }
  override def makeApplicative(argument: Term) = None

  override def toString: String = s"$variable(${arguments.map(_.toString).mkString(", ")})"
  override def serialized: String = s"with (${arguments.map(_.serialized).mkString(", ")}) $variable"
}
