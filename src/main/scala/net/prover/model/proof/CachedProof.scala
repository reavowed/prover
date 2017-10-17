package net.prover.model.proof

import java.nio.file.Path

import net.prover.model._
import net.prover.model.entries.StatementDefinition
import org.slf4j.LoggerFactory

case class CachedProof(path: Path, premises: Seq[Premise], steps: Seq[CachedStep]) {
  def getAssertionHints(availableInferences: Seq[Inference]): Seq[AssertionHint] = {
    steps.flatMap(_.getAssertionHints(availableInferences))
  }
  def validate(availableInferences: Seq[Inference], transformations: Seq[StatementDefinition]): Option[Proof] = {
    val context = ProvingContext.getInitial(premises, availableInferences, Nil, transformations)
    for {
      validatedSteps <- steps.validate(context)
    } yield Proof(validatedSteps)
  }
  def matchesOutline(outline: ProofOutline): Boolean = {
    steps.matchOutlines(outline.steps)
  }
  def serialized: String = (premises.map(_.serialized) ++ steps.flatMap(_.serializedLines)).mkString("\n")
}

object CachedProof {
  val logger = LoggerFactory.getLogger(CachedProof.getClass)

  def parser(path: Path)(implicit parsingContext: ParsingContext): Parser[CachedProof] = {
    for {
      premises <- Premise.listParser
      steps <- CachedStep.listParser(None)(parsingContext.copy(termVariableNames = parsingContext.termVariableNames + "_"))
    } yield CachedProof(path, premises, steps)
  }
}
