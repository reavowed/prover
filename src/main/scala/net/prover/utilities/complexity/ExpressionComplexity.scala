package net.prover.utilities.complexity

import scala.math.Ordering

case class ExpressionComplexity(structuralComplexity: Int, definitionComplexity: Int)

object ExpressionComplexity {
  implicit val expressionComplexityOrdering: Ordering[ExpressionComplexity] = Ordering.by(c => (c.structuralComplexity, c.definitionComplexity))
}
