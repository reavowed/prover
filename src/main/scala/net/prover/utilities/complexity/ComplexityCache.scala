package net.prover.utilities.complexity

import net.prover.model.expressions.Expression

import scala.collection.mutable

case class ComplexityCache(cache: mutable.Map[Expression, ExpressionComplexity])
