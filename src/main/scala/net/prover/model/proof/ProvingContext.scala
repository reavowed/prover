package net.prover.model.proof

import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions.Statement
import net.prover.model.{Inference, Premise}

case class ProvingContext(
  referencedFacts: Seq[ReferencedFact],
  premises: Seq[Premise],
  assumptions: Seq[Statement],
  availableInferences: Seq[Inference],
  assertionHints: Seq[AssertionHint],
  transformationStatementDefinitions: Seq[StatementDefinition],
  depth: Int)
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
  def increaseDepth(additionalDepth: Int, insertionPoint: Int) = {
    ProvingContext(
      referencedFacts.map(_.increaseDepth(additionalDepth, insertionPoint)),
      premises.map(_.increaseDepth(additionalDepth, insertionPoint)),
      assumptions.map(_.increaseDepth(additionalDepth, insertionPoint)),
      availableInferences,
      assertionHints,
      transformationStatementDefinitions,
      depth + additionalDepth)
  }
}

object ProvingContext {
  def getInitial(
    premises: Seq[Premise],
    availableInferences: Seq[Inference],
    assertionHints: Seq[AssertionHint],
    transformations: Seq[StatementDefinition]
  ): ProvingContext = {
    ProvingContext(
      premises.map(_.referencedFact),
      premises,
      Nil,
      availableInferences,
      assertionHints,
      transformations,
      0)
  }
}