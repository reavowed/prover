package net.prover.controllers.models

case class ExtractRequest(inferenceId: Option[String], serializedPremiseStatement: Option[String])
