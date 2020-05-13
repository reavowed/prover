package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.Term
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.SubstitutionContext

case class LeftDistributivity(distributor: RearrangeableOperator, distributee: RearrangeableOperator, inference: Inference.Summary, extractionOption: ExtractionOption) extends RearrangementOperation.Ternary {
  def source(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = distributor(a, distributee(b, c))
  def result(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = distributee(distributor(a, b), distributor(a, c))
}
case class RightDistributivity(distributor: RearrangeableOperator, distributee: RearrangeableOperator, inference: Inference.Summary, extractionOption: ExtractionOption) extends RearrangementOperation.Ternary {
  def source(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = distributor(distributee(a, b), c)
  def result(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = distributee(distributor(a, c), distributor(b, c))
}
