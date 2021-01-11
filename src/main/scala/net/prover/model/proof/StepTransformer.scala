package net.prover.model.proof

import net.prover.core.transformers.ExpressionTransformer

trait StepTransformer[TOutput[+_], TParameters] {
  def expressionTransformer: ExpressionTransformer[TOutput, TParameters]

}
