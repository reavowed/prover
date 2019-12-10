package net.prover.controllers.models

case class PremiseRewrite(serializedPremise: String, rewrites: Seq[Seq[RewriteRequest]])
