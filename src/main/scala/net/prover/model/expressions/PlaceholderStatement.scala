package net.prover.model.expressions

import net.prover.model.Substitutions

object PlaceholderStatement extends Statement with Placeholder[Statement] {
  override def replacePlaceholder(other: Expression) = other.asInstanceOf[Statement]
  override def calculateApplicatives(arguments: Seq[Objectable], substitutions: Substitutions) = {
    Seq((ConstantPredicate(this, 1), substitutions))
  }
  override def makeApplicative = Some(ConstantPredicate(this, 0))
  override def increaseDepth(additionalDepth: Int) = ConstantPredicate(this, additionalDepth)
}
