package net.prover.model.expressions

import monocle.macros.GenLens
import net.prover.model._

case class StatementVariable(name: String, depth: Int) extends ExpressionVariable[Statement] with Statement {
  override def increaseDepth(additionalDepth: Int) = {
    if (depth + additionalDepth < 0) throw new Exception("Invalid depth increase")
    copy(depth = depth + additionalDepth)
  }
  override def substitutionsLens = GenLens[Substitutions](_.statements)
  override def requiredSubstitutionsLens = GenLens[Substitutions.Required](_.statements)

  override def makeApplicative(names: Seq[String]) = {
    Some(PredicateApplication(name, names.mapWithIndex{ (n, i) => FunctionParameter(n, i) }, depth + 1))
  }

  override def toString: String = name
  override def serialized: String = name
}
