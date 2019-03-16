package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions.Statement
import org.slf4j.LoggerFactory

case class Proof(steps: Seq[Step]) {
  def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
  def referencedLines: Set[PreviousLineReference] = steps.flatMap(_.referencedLines).toSet
  def length: Int = steps.map(_.length).sum
  val conclusion: Statement = {
    steps.flatMap(_.provenStatements).lastOption
      .getOrElse(throw new Exception("Proof must contain at least one top-level proven statement"))
      .statement
  }
  def serializedLines = Seq("{") ++ steps.flatMap(_.serializedLines).indent ++ Seq("}")
}

object Proof {
  val logger = LoggerFactory.getLogger(Proof.getClass)
  def parser(implicit parsingContext: ParsingContext): Parser[Proof] = {
    Step.listParser(None)(parsingContext).inBraces.map(Proof.apply)
  }
}
