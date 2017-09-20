package net.prover.model.expressions

import net.prover.model.Substitutions

object PlaceholderStatement extends Statement with Placeholder[Statement] {
  override def replacePlaceholder(other: Expression) = other.asInstanceOf[Statement]
  override def calculateApplicatives(argument: Term, substitutions: Substitutions, boundVariableCount: Int) = {
    Seq((ConstantPredicate(this), substitutions))
  }
  override def makeApplicative(argument: Term) = Some(this)
}
