package net.prover.model.expressions

import monocle.macros.GenLens
import net.prover.model._

case class StatementVariable(name: String) extends ExpressionVariable[Statement] with Substitutions.Lenses.ForStatements with Statement {
  override def getTerms(depth: Int) = Nil
}
