package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.Term
import net.prover.model.proof.SubstatementExtractor.ExtractionOption
import net.prover.model.proof.SubstitutionContext

case class Commutativity(operator: BinaryOperator, inference: Inference.Summary, extractionOption: ExtractionOption) extends RearrangementOperation.Binary {
  override def source(a: Term, b: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(a, b)
  override def result(a: Term, b: Term)(implicit substitutionContext: SubstitutionContext): Term = operator(b, a)
}
