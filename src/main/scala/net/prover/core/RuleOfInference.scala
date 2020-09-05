package net.prover.core

import net.prover.core.expressions.Statement

trait RuleOfInference {
  def premises: Seq[Statement]
  def conclusion: Statement
}

object RuleOfInference {
  case class Raw(premises: Seq[Statement], conclusion: Statement) extends RuleOfInference
}
