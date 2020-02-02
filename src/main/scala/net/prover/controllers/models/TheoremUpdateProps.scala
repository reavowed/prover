package net.prover.controllers.models

import net.prover.model.entries.Theorem

case class TheoremUpdateProps(theorem: Theorem, newInferences: Map[String, InferenceSummary])
