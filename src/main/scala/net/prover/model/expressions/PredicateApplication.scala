package net.prover.model.expressions
import net.prover.model.Substitutions

case class PredicateApplication(variable: PredicateVariable, arguments: Seq[Term]) extends Statement {
  override def requiredSubstitutions = arguments.requiredSubstitutions ++ Substitutions.Required(Nil, Seq(variable))
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions) = {
    other match {
      case assertable: Assertable =>
        assertable
          .calculateApplicatives(arguments, substitutions)
          .flatMap { case (predicate, newSubstitutions) =>
            newSubstitutions.addPredicate(variable, predicate)
          }
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions) = {
    for {
      predicate <- substitutions.predicatesByName.get(variable)
      updatedArguments <- arguments.applySubstitutions(substitutions).map(_.map(_.asInstanceOf[Objectable]))
    } yield predicate(updatedArguments)
  }
  override def replacePlaceholder(other: Expression) = copy(arguments = arguments.map(_.replacePlaceholder(other)))
  override def calculateApplicatives(targetArguments: Seq[Objectable], substitutions: Substitutions) = {
    arguments.calculateApplicatives(targetArguments, substitutions).map(_.mapLeft { functions =>
      MetaPredicateApplication(variable, functions.map(_.asInstanceOf[Function]), 1)
    })
  }
  override def makeApplicative = None
  override def increaseDepth(additionalDepth: Int) = {
    MetaPredicateApplication(variable, arguments.map(_.increaseDepth(additionalDepth)), additionalDepth)
  }

  override def toString: String = s"${variable.name}(${arguments.map(_.toString).mkString(", ")})"
  override def serialized: String = s"with (${arguments.map(_.serialized).mkString(", ")}) ${variable.name}"
}
