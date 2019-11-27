package net.prover.controllers.models

case class SubstitutionRequest(inferenceId: String, serializedPremises: Map[String, String], withConclusion: Boolean)
