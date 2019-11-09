package net.prover.controllers.models

case class StepDefinition(inferenceId: String, substitutions: SerializedSubstitutions, rewriteInferenceId: Option[String])
