package net.prover.controllers.models

import net.prover.model.proof.Step

case class StepUpdateProps(path: Seq[Int], step: Step, newInferences: Map[String, InferenceSummary])
