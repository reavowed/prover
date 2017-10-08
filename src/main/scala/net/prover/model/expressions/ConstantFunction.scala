package net.prover.model.expressions

import net.prover.model.Substitutions

case class ConstantFunction(term: Objectable, depth: Int) extends Function {
  override def apply(arguments: Seq[Objectable]) = {
    if (depth == 1)
      term
    else
      ConstantFunction(term, depth - 1)
  }
  override def increaseDepth(additionalDepth: Int) = {
    ConstantFunction(term, depth + additionalDepth)
  }

  override def requiredSubstitutions = term.requiredSubstitutions
  override def calculateSubstitutions(other: Expression, substitutions: Substitutions) = {
    other match {
      case ConstantFunction(otherTerm, `depth`) =>
        term.calculateSubstitutions(otherTerm, substitutions)
      case ConstantFunction(otherTerm, higherDepth) if higherDepth > depth =>
        term.calculateSubstitutions(ConstantFunction(otherTerm, higherDepth - depth), substitutions)
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions) = {
    for {
      substitutedTerm <- term.applySubstitutions(substitutions)
    } yield substitutedTerm.increaseDepth(depth)
  }
  override def calculateApplicatives(arguments: Seq[Objectable], substitutions: Substitutions): Seq[(Function, Substitutions)] = {
    super.calculateApplicatives(arguments, substitutions) ++ Seq(ConstantFunction(term, depth + 1) -> substitutions)
  }
  override def replacePlaceholder(other: Expression) = copy(term.replacePlaceholder(other))

  override def serialized = term.serialized
  override def toString = (1 to depth).map(_ => "$").mkString("") + term.toString
  override def safeToString = (1 to depth).map(_ => "$").mkString("") + term.safeToString
}
