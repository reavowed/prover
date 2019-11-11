package net.prover.model.expressions

import monocle.macros.GenLens
import net.prover.model._

case class StatementVariable(name: String) extends ExpressionVariable[Statement] with Statement {
  override def substitutionsLens = GenLens[Substitutions](_.statements)
  override def possibleSubstitutionsLens = GenLens[Substitutions.Possible](_.statements)
  override def requiredSubstitutionsLens = GenLens[Substitutions.Required](_.statements)
  override def getTerms(depth: Int) = Nil
}
