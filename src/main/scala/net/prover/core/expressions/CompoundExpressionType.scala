package net.prover.core.expressions

sealed trait CompoundExpressionType {
  def symbol: String
  def hasBoundVariables: Boolean
}

trait CompoundStatementType extends CompoundExpressionType {
  override def equals(obj: Any): Boolean = obj match {
    case other: CompoundStatementType =>
      symbol == other.symbol
    case _ =>
      false
  }
  override def hashCode(): Int = symbol.hashCode()
}

trait CompoundTermType extends CompoundExpressionType {
  override def equals(obj: Any): Boolean = obj match {
    case other: CompoundTermType =>
      symbol == other.symbol
    case _ =>
      false
  }
  override def hashCode(): Int = symbol.hashCode()
}
