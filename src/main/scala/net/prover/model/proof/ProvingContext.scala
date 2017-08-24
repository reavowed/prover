package net.prover.model.proof

import net.prover.model.{Inference, Premise}
import net.prover.model.components.Statement

case class ProvingContext(
  referencedFacts: Seq[ReferencedFact],
  premises: Seq[Premise],
  assumptions: Seq[Statement],
  availableInferences: Seq[Inference],
  assertionHints: Seq[AssertionHint])
{
  def addFact(fact: Fact, reference: Reference.Direct) = {
    copy(referencedFacts = referencedFacts :+ ReferencedFact(fact, reference))
  }
  def addFact(referencedFact: ReferencedFact) = {
    copy(referencedFacts = referencedFacts :+ referencedFact)
  }
  def addFact(referencedFact: Option[ReferencedFact]) = {
    copy(referencedFacts = referencedFacts ++ referencedFact.toSeq)
  }
}
