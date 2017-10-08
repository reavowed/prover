package net.prover.model.expressions

import net.prover.model.Substitutions

case class MetaPredicateApplication(variable: PredicateVariable, arguments: Seq[Function], depth: Int) extends Predicate {
  override def apply(innerArguments: Seq[Objectable]) = {
    if (depth == 1) {
      PredicateApplication(variable, arguments.map(_(innerArguments).asInstanceOf[Term]))
    } else {
      MetaPredicateApplication(variable, arguments.map(_(innerArguments).asInstanceOf[Function]), depth - 1)
    }
  }
  override def increaseDepth(additionalDepth: Int) = {
    MetaPredicateApplication(
      variable,
      arguments.map(_.increaseDepth(additionalDepth)),
      depth + additionalDepth)
  }

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
      updatedArguments <- arguments.applySubstitutions(substitutions).map(_.map(_.asInstanceOf[Function]))
      depthChange = updatedArguments.headOption.map(_.depth).getOrElse(0)
    } yield {
      predicate(updatedArguments).asInstanceOf[Predicate]
    }
  }
  override def calculateApplicatives(targetArguments: Seq[Objectable], substitutions: Substitutions) = {
    arguments.calculateApplicatives(targetArguments, substitutions).map(_.mapLeft { functions =>
      MetaPredicateApplication(variable, functions.map(_.asInstanceOf[Function]), depth + 1)
    })
  }
  override def replacePlaceholder(other: Expression) = copy(arguments = arguments.map(_.replacePlaceholder(other)))

  override def serialized = s"with (${arguments.map(_.serialized).mkString(", ")}) ${variable.name}"
  override def toString = s"${variable.name}(${arguments.map(_.toString).mkString(", ")})"
}
