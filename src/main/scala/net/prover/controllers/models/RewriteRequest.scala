package net.prover.controllers.models

case class RewriteRequest(path: Seq[Int], inferenceId: String, reverse: Boolean)
