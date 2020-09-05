package net.prover.core.expressions

sealed trait ExpressionComponentType
object ExpressionComponentType {
  case object Statement extends ExpressionComponentType
  case object Term extends ExpressionComponentType
}
