package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions.Statement
import org.slf4j.LoggerFactory

case class Proof(steps: Seq[Step]) {
  def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
  def length: Int = steps.map(_.length).sum
  def serializedLines = Seq("{") ++ steps.flatMap(_.serializedLines).indent ++ Seq("}")
}

object Proof {
  val logger = LoggerFactory.getLogger(Proof.getClass)
  def parser(premises: Seq[Premise])(implicit parsingContext: ParsingContext): Parser[Proof] = {
    Step.listParser(None)(parsingContext, StepContext(premises.map(_.provenStatement), 0)).inBraces.map(Proof.apply)
  }
}
