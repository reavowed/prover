package net.prover.model.definitions

import net.prover.model.expressions.Term
import net.prover.proving.extraction.SubstatementExtractor.InferenceExtraction

case class TermRewriteInference(inferenceExtraction: InferenceExtraction, lhs: Term, rhs: Term) extends DerivedInference
