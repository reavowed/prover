package net.prover.model.definitions

import net.prover.model.Substitutions
import net.prover.model.expressions.Statement

case class BinaryConnective(symbol: String, template: Statement, attributes: Seq[String]) extends BinaryJoiner[Statement] with Substitutions.Lenses.ForStatements
