package net.prover.model.expressions

import monocle.macros.GenLens
import net.prover.model._

case class StatementVariable(name: String, depth: Int) extends ExpressionVariable[Statement] with Statement {
  override def setDepth(newDepth: Int) = {
    copy(depth = newDepth)
  }
  override def substitutionsLens = GenLens[Substitutions](_.statements)
  override def requiredSubstitutionsLens = GenLens[Substitutions.Required](_.statements)

  override def toString: String = name
  override def serialized: String = name
}
