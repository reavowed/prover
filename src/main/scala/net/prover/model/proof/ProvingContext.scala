package net.prover.model.proof

import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model.{Inference, Premise}

case class ProvingContext(
  referencedFacts: Seq[ReferencedFact],
  premises: Seq[Premise],
  assumptions: Seq[Statement],
  availableInferences: Seq[Inference],
  assertionHints: Seq[AssertionHint],
  deductionStatement: Option[StatementDefinition],
  scopingStatement: Option[StatementDefinition],
  depth: Int,
  allowTransformations: Boolean = true)
{
  def addFact(statement: Statement, reference: Reference.Direct) = {
    copy(referencedFacts = referencedFacts :+ ReferencedFact(statement, reference))
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
      deductionStatement,
      scopingStatement,
      depth + additionalDepth)
  }

  def deduced(antecedent: Statement, consequent: Statement): Option[Statement] = {
    deductionStatement.map { definition =>
      DefinedStatement(Seq(antecedent, consequent), definition, antecedent.depth - 1)(Nil)
    }
  }

  def scoped(inner: Statement, variableName: String): Option[Statement] = {
    scopingStatement.map { definition =>
      DefinedStatement(Seq(inner), definition, inner.depth - 1)(Seq(variableName))
    }
  }
}

object ProvingContext {
  def getInitial(
    premises: Seq[Premise],
    assertionHints: Seq[AssertionHint],
    proofEntries: ProofEntries
  ): ProvingContext = {
    getInitial(premises, assertionHints, proofEntries.availableInferences, proofEntries.statementDefinitions)
  }

  def getInitial(
    premises: Seq[Premise],
    assertionHints: Seq[AssertionHint],
    availableInferences: Seq[Inference],
    statementDefinitions: Seq[StatementDefinition]
  ): ProvingContext = {
    ProvingContext(
      premises.map(_.referencedFact),
      premises,
      Nil,
      availableInferences,
      assertionHints,
      statementDefinitions.find(_.structureType.contains(StatementDefinition.StructureType.Deduction)),
      statementDefinitions.find(_.structureType.contains(StatementDefinition.StructureType.Scoping)),
      0)
  }
}