package net.prover.model.proof

import net.prover.model.{Inference, Premise}
import net.prover.model.expressions.{Assertable, Statement}
import net.prover.model.entries.StatementDefinition

case class ProvingContext(
  referencedFacts: Seq[ReferencedFact],
  premises: Seq[Premise],
  assumptions: Seq[Assertable],
  availableInferences: Seq[Inference],
  assertionHints: Seq[AssertionHint],
  transformationStatementDefinitions: Seq[StatementDefinition])
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
  def increaseDepth(additionalDepth: Int) = {
    ProvingContext(
      referencedFacts.map(_.increaseDepth(additionalDepth)),
      premises.map(_.increaseDepth(additionalDepth)),
      assumptions.map(_.increaseDepth(additionalDepth)),
      availableInferences,
      assertionHints,
      transformationStatementDefinitions)
  }
}
