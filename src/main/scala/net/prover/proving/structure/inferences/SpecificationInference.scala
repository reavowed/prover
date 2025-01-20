package net.prover.proving.structure.inferences;

import net.prover.model.Inference
import net.prover.model.expressions.Statement
import net.prover.proving.structure.definitions.GeneralizationDefinition;

case class SpecificationInference(inference: Inference, premise: Statement, generalizationDefinition: GeneralizationDefinition)
