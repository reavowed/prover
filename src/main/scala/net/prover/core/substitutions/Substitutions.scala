package net.prover.core.substitutions

import net.prover.core.expressions.{Statement, Term}

case class Substitutions(statements: Seq[Statement], terms: Seq[Term])

object Substitutions {
  val empty: Substitutions = Substitutions(Nil, Nil)
}
