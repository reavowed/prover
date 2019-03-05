package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions.Statement
import org.slf4j.LoggerFactory

case class Proof(steps: Seq[Step]) {
  def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
  def referenceMap: ReferenceMap = steps.map(_.referenceMap).foldTogether
  def length: Int = steps.map(_.length).sum
  val conclusion: Option[Statement] = steps.flatMap(_.provenStatements).lastOption.map(_.statement)

  def getLines(implicit displayContext: DisplayContext): Seq[ProofLine] = steps.flatMap(_.getLines(referenceMap, 0, None))
  def serializedLines = Seq("{") ++ steps.flatMap(_.serializedLines).indent ++ Seq("}")
}

object Proof {
  val logger = LoggerFactory.getLogger(Proof.getClass)
  def parser(implicit parsingContext: ParsingContext): Parser[Proof] = {
    Step.listParser(None)(parsingContext.copy(termVariableNames = parsingContext.termVariableNames + "_")).inBraces.map(Proof.apply)
  }
}
