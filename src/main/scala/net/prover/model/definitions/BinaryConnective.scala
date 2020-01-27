package net.prover.model.definitions

import net.prover.model.Substitutions
import net.prover.model.expressions.Statement

case class BinaryConnective(template: Statement, attributes: Seq[String]) extends BinaryStatement[Statement] with Substitutions.Lenses.ForStatements
