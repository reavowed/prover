package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions.Statement
import org.slf4j.LoggerFactory

case class Proof(steps: Seq[Step]) {
  def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
  def referenceMap: ReferenceMap = steps.map(_.referenceMap).foldTogether
  val conclusion: Statement = {
    steps.ofType[Step.WithAssertion].lastOption
      .flatMap(_.assertion.asOptionalInstanceOf[Statement])
      .getOrElse(throw new Exception("Proof must contain at least one top-level proven statement"))
  }
}

object Proof {
  val logger = LoggerFactory.getLogger(Proof.getClass)
}
