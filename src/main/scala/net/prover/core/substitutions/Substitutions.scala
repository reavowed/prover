package net.prover.core.substitutions

import net.prover.core.expressions.{Statement, Term}

case class Substitutions(statements: Seq[Statement], terms: Seq[Term]) {
  def applier: SubstitutionApplier = SubstitutionApplier(this)
  def specifier: SubstitutionSpecifier = SubstitutionSpecifier(this)
}

