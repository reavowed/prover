package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.Term
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.SubstitutionContext

case class Associativity(operator: BinaryOperator, inference: Inference.Summary, extractionOption: ExtractionOption) extends RearrangementOperation.Ternary {
  def source(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(a, operator(b, c))
  def result(a: Term, b: Term, c: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(operator(a, b), c)
}
