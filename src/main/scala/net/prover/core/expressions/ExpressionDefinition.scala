package net.prover.core.expressions

sealed trait ExpressionDefinition
case class StatementDefinition(componentTypes: Seq[ExpressionComponentType])
case class TermDefinition(componentTypes: Seq[ExpressionComponentType])
