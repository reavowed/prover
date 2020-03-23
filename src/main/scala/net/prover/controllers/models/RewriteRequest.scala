package net.prover.controllers.models

case class RewriteRequest(path: Seq[Int], inferenceId: Option[String], serializedPremiseStatement: Option[String], extractionInferenceIds: Seq[String], reverse: Boolean)
