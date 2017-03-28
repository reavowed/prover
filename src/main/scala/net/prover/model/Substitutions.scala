package net.prover.model

import scala.util.Try

case class Substitutions(
    statements: Map[StatementVariable, Statement],
    terms: Map[TermVariable, Term])

case class PartialSubstitutions(
    knownStatements: Map[StatementVariable, Statement],
    knownTerms: Map[TermVariable, Term],
    unknownSubstitutions: Map[StatementVariableWithSingleSubstitution, Statement]) {

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
    statementVariableWithSingleSubstitution: StatementVariableWithSingleSubstitution,
    statement: Statement
  ): Option[PartialSubstitutions] = {
    unknownSubstitutions.keySet.find(
      s => s.statementVariable == statementVariableWithSingleSubstitution.statementVariable
    ) match {
      case Some(other) =>
        if (other.termToBeReplaced == statementVariableWithSingleSubstitution.termToBeReplaced) {
          statement.resolveSingleSubstitution(
            unknownSubstitutions(other),
            statementVariableWithSingleSubstitution.termToBeReplaced,
            statementVariableWithSingleSubstitution.termToReplaceWith,
            other.termToReplaceWith
          ).flatMap { resolvedStatement =>
            copy(unknownSubstitutions = unknownSubstitutions - other)
              .tryAdd(statementVariableWithSingleSubstitution.statementVariable, resolvedStatement)
          }
        } else {
          None // TODO: overly conservative
        }
      case None =>
        Some(
          tryAddingByCalculatingTermToReplaceWith(statementVariableWithSingleSubstitution, statement)
            .getOrElse(copy(unknownSubstitutions = unknownSubstitutions + (statementVariableWithSingleSubstitution -> statement))))
    }
  }


  private def tryAddingByCalculatingTermToReplaceWith(
    statementVariableWithSingleSubstitution: StatementVariableWithSingleSubstitution,
    statement: Statement
  ): Option[PartialSubstitutions] = {
    (for {
      mappedStatementVariable <- knownStatements.get(statementVariableWithSingleSubstitution.statementVariable)
      mappedTermToBeReplaced <- knownTerms.get(statementVariableWithSingleSubstitution.termToBeReplaced)
        .flatMap(t => Try(Term.asVariable(t)).toOption)
      mappedTermToReplaceWithOption <- mappedStatementVariable.findSubstitution(statement, mappedTermToBeReplaced)
    } yield mappedTermToReplaceWithOption)
      .flatten
      .flatMap(t => statementVariableWithSingleSubstitution.termToReplaceWith.calculateSubstitutions(t, this))
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
    val resolvedSubstitutions = Substitutions(knownStatements, knownTerms)
    val allResolved = unknownSubstitutions.forall {
      case (statementVariableWithSubstitutions, statement) =>
        Try(statementVariableWithSubstitutions.applySubstitutions(resolvedSubstitutions)).toOption.contains(statement)
    }
    if (allResolved) {
      Some(resolvedSubstitutions)
    } else {
      None
    }
  }
}

object PartialSubstitutions {
  val empty: PartialSubstitutions = PartialSubstitutions(Map.empty, Map.empty, Map.empty)
}
