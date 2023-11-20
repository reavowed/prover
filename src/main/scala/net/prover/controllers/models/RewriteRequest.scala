package net.prover.controllers.models

import net.prover.proving.extraction.ExtractionDefinition

case class RewriteRequest(path: Seq[Int], inferenceId: Option[String], serializedPremiseStatement: Option[String], extractionDefinition: ExtractionDefinition.Serialized)
