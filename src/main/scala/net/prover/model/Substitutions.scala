package net.prover.model

import scala.util.Try

case class Substitutions(
    statements: Map[StatementVariable, Statement],
    terms: Map[TermVariable, Term])

case class PartialSubstitutions(
    knownStatements: Map[StatementVariable, Statement],
    knownTerms: Map[TermVariable, Term],
    unknownSubstitutions: Map[SubstitutedStatementVariable, Statement]) {

  def tryAdd(statementVariable: StatementVariable, statement: Statement): Option[PartialSubstitutions] = {
    knownStatements.get(statementVariable) match {
      case Some(`statement`) =>
        Some(this)
      case Some(otherStatement) =>
        None
      case None =>
        Some(copy(knownStatements = knownStatements + (statementVariable -> statement)))
    }
  }

  def tryAdd(
    substitutedStatementVariable: SubstitutedStatementVariable,
    statement: Statement
  ): Option[PartialSubstitutions] = {
    Some(
      tryAddDirectly(substitutedStatementVariable, statement)
        .getOrElse(copy(unknownSubstitutions = unknownSubstitutions + (substitutedStatementVariable -> statement)))
    )
  }

  private def tryAddDirectly(
    substitutedStatementVariable: SubstitutedStatementVariable,
    statement: Statement
  ): Option[PartialSubstitutions] = {
    unknownSubstitutions.keySet.find(
      s => s.statementVariable == substitutedStatementVariable.statementVariable
    ) match {
      case Some(other) =>
        tryAddingByMerge(substitutedStatementVariable, other, statement)
      case None =>
        tryAddingByCalculatingTermToReplaceWith(substitutedStatementVariable, statement)
    }
  }

  private def tryAddingByMerge(
    substitutedStatementVariable: SubstitutedStatementVariable,
    otherSubstitutedStatementVariable: SubstitutedStatementVariable,
    statement: Statement
  ): Option[PartialSubstitutions] = {
    if (otherSubstitutedStatementVariable.termToBeReplaced == substitutedStatementVariable.termToBeReplaced) {
      statement.resolveSingleSubstitution(
        unknownSubstitutions(otherSubstitutedStatementVariable),
        substitutedStatementVariable.termToBeReplaced,
        substitutedStatementVariable.termToReplaceWith,
        otherSubstitutedStatementVariable.termToReplaceWith
      ).flatMap { resolvedStatement =>
        copy(unknownSubstitutions = unknownSubstitutions - otherSubstitutedStatementVariable)
          .tryAdd(substitutedStatementVariable.statementVariable, resolvedStatement)
      }
    } else {
      None // TODO: overly conservative
    }
  }

  private def tryAddingByCalculatingTermToReplaceWith(
    substitutedStatementVariable: SubstitutedStatementVariable,
    statement: Statement
  ): Option[PartialSubstitutions] = {
    (for {
      mappedStatementVariable <- knownStatements.get(substitutedStatementVariable.statementVariable)
      mappedTermToBeReplaced <- knownTerms.get(substitutedStatementVariable.termToBeReplaced)
        .flatMap(t => Try(Term.asVariable(t)).toOption)
      mappedTermToReplaceWithOption <- mappedStatementVariable.findSubstitution(statement, mappedTermToBeReplaced)
    } yield mappedTermToReplaceWithOption)
      .flatten
      .flatMap(t => substitutedStatementVariable.termToReplaceWith.calculateSubstitutions(t, this))
  }

  def tryAdd(termVariable: TermVariable, term: Term): Option[PartialSubstitutions] = {
    knownTerms.get(termVariable) match {
      case Some(`term`) =>
        Some(this)
      case Some(otherTerm) =>
        None
      case None =>
        Some(copy(knownTerms = knownTerms + (termVariable -> term)))
    }
  }

  def tryResolve(): Option[Substitutions] = {
    unknownSubstitutions.keys.toList match {
      case Nil =>
        Some(Substitutions(knownStatements, knownTerms))
      case one +: more =>
        copy(unknownSubstitutions = unknownSubstitutions.filterKeys(more.contains))
          .tryAddDirectly(one, unknownSubstitutions(one))
          .flatMap(_.tryResolve())
    }
  }
}

object PartialSubstitutions {
  val empty: PartialSubstitutions = PartialSubstitutions(Map.empty, Map.empty, Map.empty)
}
