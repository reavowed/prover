package net.prover.core.expressions

sealed trait CompoundExpressionComponentType
object CompoundExpressionComponentType {
  case object Statement extends CompoundExpressionComponentType
  case object Term extends CompoundExpressionComponentType
}
