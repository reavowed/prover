package net.prover.model.definitions

import net.prover.model.Inference
import net.prover.model.expressions.Statement

case class NamingInference(
  baseInference: Inference,
  premises: Seq[Statement],
  assumption: Statement,
  generalizationDefinition: GeneralizationDefinition,
  deductionDefinition: DeductionDefinition)
