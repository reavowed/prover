package net.prover.proving.rewrite

import net.prover.model.Inference
import net.prover.model.expressions.{Expression, Term, TypedExpression}
import net.prover.model.proof.EqualityRewriter.RewriteMethods
import net.prover.model.unwrapping.Unwrapper

case class RewriteStep[TExpression <: Expression with TypedExpression[TExpression] : RewriteMethods](
  baseTerm: Term,
  rewrittenTerm: Term,
  premise: RewritePremise,
  inferenceOption: Option[Inference.Summary],
  fallbackInferenceOption: Option[Inference.Summary],
  unwrappers: Seq[Unwrapper],
  wrapperExpression: TExpression)