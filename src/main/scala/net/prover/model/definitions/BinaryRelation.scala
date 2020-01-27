package net.prover.model.definitions

import net.prover.model._
import net.prover.model.expressions.{Statement, Term}

case class BinaryRelation(template: Statement, attributes: Seq[String]) extends BinaryStatement[Term] with Substitutions.Lenses.ForTerms
