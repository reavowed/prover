package net.prover.controllers.models

case class RewriteRequest(path: Seq[Int], inferenceId: String, direction: String)
