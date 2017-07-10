package net.prover.model

import net.prover.model.DetailedProof.{ReferencedAssertion, ReferencedDeduction}
import net.prover.model.Inference.Premise
import net.prover.model.components.Statement

case class ProvingContext(
  provenAssertions: Seq[ReferencedAssertion],
  provenDeductions: Seq[ReferencedDeduction],
  premises: Seq[Premise],
  assumptions: Seq[Statement],
  availableInferences: Seq[Inference],
  inferenceTransforms: Seq[InferenceTransform],
  bookName: String,
  theoremName: String)
