package net.prover.proving.structure.inferences

import net.prover.model.Inference
import net.prover.model.expressions.Statement;

case class DeductionEliminationInference(inference: Inference, deductionPremise: Statement, antecedentPremise: Statement)
