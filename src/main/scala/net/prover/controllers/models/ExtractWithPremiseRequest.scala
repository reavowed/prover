package net.prover.controllers.models

case class ExtractWithPremiseRequest(inferenceId: Option[String], serializedBasePremiseStatement: Option[String], serializedHelperPremiseStatement: String)
