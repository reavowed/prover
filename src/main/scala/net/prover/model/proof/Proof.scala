package net.prover.model.proof

import net.prover.model.expressions.Statement
import org.slf4j.LoggerFactory

case class Proof(steps: Seq[Step]) {
  def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
  def referenceMap: ReferenceMap = steps.map(_.referenceMap).foldTogether
  def length: Int = steps.map(_.length).sum
  val conclusion: Statement = {
    steps.flatMap(_.facts).lastOption
      .getOrElse(throw new Exception("Proof must contain at least one top-level proven statement"))
      .statement
  }
}

object Proof {
  val logger = LoggerFactory.getLogger(Proof.getClass)
}
