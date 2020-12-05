package net.prover.core.expressions

sealed trait CompoundExpressionType
case class CompoundStatementType(componentTypes: Seq[CompoundExpressionComponentType])
case class CompoundTermType(componentTypes: Seq[CompoundExpressionComponentType])
