package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.Term
import net.prover.model.proof.SubstatementExtractor.ExtractionOption

case class TermRewriteInference(inference: Inference, extractionOption: ExtractionOption, lhs: Term, rhs: Term) {
  def inferenceSummary: Inference.Summary = Inference.Summary(inference.name, inference.id, extractionOption.premises, extractionOption.conclusion, extractionOption.additionalVariableNames)
}
