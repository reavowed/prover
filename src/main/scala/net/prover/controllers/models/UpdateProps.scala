package net.prover.controllers.models

import net.prover.model.entries.Theorem
import net.prover.model.proof.Step

sealed trait UpdateProps

case class TheoremUpdateProps(theorem: Theorem, newInferences: Map[String, InferenceSummary]) extends UpdateProps
case class ProofUpdateProps(proof: Seq[Step], newInferences: Map[String, InferenceSummary]) extends UpdateProps
case class StepUpdateProps(path: Seq[Int], step: Step, newInferences: Map[String, InferenceSummary]) extends UpdateProps
