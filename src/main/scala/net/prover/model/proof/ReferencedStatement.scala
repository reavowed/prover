package net.prover.model.proof

import net.prover.model.expressions.Statement

case class ReferencedStatement(statement: Statement, reference: Reference)