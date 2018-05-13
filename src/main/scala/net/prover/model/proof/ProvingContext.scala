package net.prover.model.proof

import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model.{Inference, Premise, Substitutions}

case class ProvingContext(
  provenStatements: Seq[ProvenStatement],
  premises: Seq[Premise],
  assumptions: Seq[Statement],
  availableInferences: Seq[Inference],
  assertionHints: Seq[CachedStep.Assertion],
  deductionStatement: Option[StatementDefinition],
  scopingStatement: Option[StatementDefinition],
  depth: Int,
  allowTransformations: Boolean = true)
{
  def resetWithPremises(newPremises: Seq[Premise]): ProvingContext = {
    ProvingContext.getInitial(newPremises, assertionHints, availableInferences, deductionStatement.toSeq ++ scopingStatement.toSeq)
  }
  def addProvenStatement(statement: Statement, reference: Reference.Direct) = {
    copy(provenStatements = provenStatements :+ ProvenStatement(statement, reference))
  }
  def addProvenStatement(newProvenStatements: Seq[ProvenStatement]) = {
    copy(provenStatements = provenStatements ++ newProvenStatements)
  }
  def insertExternalParameter() = {
    ProvingContext(
      provenStatements.map(_.insertExternalParameters(1)),
      premises.map(_.insertExternalParameters(1)),
      assumptions.map(_.insertExternalParameters(1)),
      availableInferences,
      assertionHints,
      deductionStatement,
      scopingStatement,
      depth + 1)
  }

  def deduced(antecedent: Statement, consequent: Statement): Option[Statement] = {
    deductionStatement.map { definition =>
      DefinedStatement(Seq(antecedent, consequent), definition)(Nil)
    }
  }

  def scoped(inner: Statement, variableName: String): Option[Statement] = {
    scopingStatement.map { definition =>
      DefinedStatement(Seq(inner), definition)(Seq(variableName))
    }
  }

  override def toString = s"ProvingContext[${provenStatements.length} statements]"
}

object ProvingContext {
  def getInitial(
    premises: Seq[Premise],
    assertionHints: Seq[CachedStep.Assertion],
    proofEntries: ProofEntries
  ): ProvingContext = {
    getInitial(premises, assertionHints, proofEntries.availableInferences, proofEntries.statementDefinitions)
  }

  def getInitial(
    premises: Seq[Premise],
    assertionHints: Seq[CachedStep.Assertion],
    availableInferences: Seq[Inference],
    statementDefinitions: Seq[StatementDefinition],
    depth: Int = 0
  ): ProvingContext = {
    ProvingContext(
      premises.map(_.provenStatement),
      premises,
      Nil,
      availableInferences,
      assertionHints,
      statementDefinitions.find(_.structureType.contains(StatementDefinition.StructureType.Deduction)),
      statementDefinitions.find(_.structureType.contains(StatementDefinition.StructureType.Scoping)),
      depth)
  }
}
