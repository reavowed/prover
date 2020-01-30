package net.prover.model.definitions

import net.prover.model._
import net.prover.model.expressions.{Statement, Term}

case class BinaryRelation(symbol: String, template: Statement, attributes: Seq[String]) extends BinaryJoiner[Term] with Substitutions.Lenses.ForTerms
