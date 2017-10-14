package net.prover.model.expressions

import net.prover.model.Substitutions

case class ArgumentList(terms: Seq[Term], depth: Int) {
  def apply(index: Int): Term = terms(index)

  def increaseDepth(additionalDepth: Int, insertionPoint: Int) = {
    ArgumentList(terms.map(_.increaseDepth(additionalDepth, insertionPoint)), depth + additionalDepth)
  }
  def reduceDepth(difference: Int, insertionPoint: Int) = {
    terms
      .map(_.reduceDepth(difference, insertionPoint)).traverseOption
      .map(ArgumentList(_, depth - difference))
  }
  def specify(targetArguments: ArgumentList) = {
    if (depth == 0) throw new Exception("Cannot specify base-level expression")
    ArgumentList(terms.map(_.specify(targetArguments)), depth - 1)
  }
  def specifyWithSubstitutions(
    targetArguments: ArgumentList,
    substitutions: Substitutions,
    outerDepth: Int
  ) = {
    if (depth == 0) throw new Exception("Cannot specify base-level expression")
    terms
      .map(_.specifyWithSubstitutions(targetArguments, substitutions, outerDepth)).traverseOption
      .map(ArgumentList(_, depth + outerDepth - 1))
  }

  def requiredSubstitutions = terms.requiredSubstitutions
  def calculateSubstitutions(other: ArgumentList, substitutions: Substitutions) = {
    terms.calculateSubstitutions(other.terms, substitutions)
  }
  def applySubstitutions(substitutions: Substitutions) = {
    terms
      .map(_.applySubstitutions(substitutions)).traverseOption
      .map(ArgumentList(_, depth + substitutions.depth))
  }
  def calculateApplicatives(baseArguments: ArgumentList, substitutions: Substitutions): Seq[(ArgumentList, Substitutions)] = {
    terms.calculateApplicatives(baseArguments, substitutions)
      .map(_.mapLeft(ts => ArgumentList(ts.map(_.asInstanceOf[Term]), depth - baseArguments.depth + 1)))
  }

}
