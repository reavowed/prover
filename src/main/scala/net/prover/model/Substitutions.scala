package net.prover.model

case class Substitutions(
    statements: Map[StatementVariable, Statement],
    terms: Map[TermVariable, Term])

case class PartialSubstitutions(
    knownStatements: Map[StatementVariable, Statement],
    knownTerms: Map[TermVariable, Term],
    unknownSubstitutions: Map[SubstitutedStatementVariable, Statement],
    distinctVariables: Map[TermVariable, Variables]) {

  private def knownSubstitutions = Substitutions(knownStatements, knownTerms)

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

  def tryAdd(termVariable: TermVariable, term: Term): Option[PartialSubstitutions] = {
    knownTerms.get(termVariable) match {
      case Some(`term`) =>
        Some(this)
      case Some(_) =>
        None
      case None =>
        Some(copy(knownTerms = knownTerms + (termVariable -> term)))
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
    tryNotAddingAtAll(substitutedStatementVariable, statement)
      .orElse(tryAddingByCalculatingTermToReplaceWith(substitutedStatementVariable, statement))
      .orElse(tryAddingByMerge(substitutedStatementVariable, statement))
  }

  private def tryNotAddingAtAll(
    substitutedStatementVariable: SubstitutedStatementVariable,
    statement: Statement
  ): Option[PartialSubstitutions] = {
    for {
      mappedStatementVariable <- knownStatements.get(substitutedStatementVariable.statementVariable)
      mappedTermToBeReplaced <- knownTerms.get(substitutedStatementVariable.termToBeReplaced).flatMap(Term.optionAsVariable)
      mappedTermToReplaceWith <- substitutedStatementVariable.termToReplaceWith.applySubstitutions(knownSubstitutions)
      mappedStatement = mappedStatementVariable.makeSingleSubstitution(mappedTermToReplaceWith, mappedTermToBeReplaced)
      if mappedStatement == statement
    } yield this
  }

  private def tryAddingByMerge(
    thisSubstitutedStatementVariable: SubstitutedStatementVariable,
    thisTarget: Statement
  ): Option[PartialSubstitutions] = {
    for {
      otherSubstitutedStatementVariable <- unknownSubstitutions.keySet
        .find { s =>
          s.statementVariable == thisSubstitutedStatementVariable.statementVariable &&
          s.termToBeReplaced == thisSubstitutedStatementVariable.termToBeReplaced
        }
      otherTarget = unknownSubstitutions(otherSubstitutedStatementVariable)
      sharedTermToBeReplaced = otherSubstitutedStatementVariable.termToBeReplaced
      thisTermToReplaceWith <- thisSubstitutedStatementVariable.termToReplaceWith.applySubstitutions(knownSubstitutions)
      otherTermToReplaceWith <- otherSubstitutedStatementVariable.termToReplaceWith.applySubstitutions(knownSubstitutions)
      placeholderVariableTerm = knownTerms.getOrElse(
        sharedTermToBeReplaced,
        sharedTermToBeReplaced.copy(text = sharedTermToBeReplaced + "'"))
      placeholderVariable <- Term.optionAsVariable(placeholderVariableTerm)
      resolvedStatement <- thisTarget.resolveSingleSubstitution(
        otherTarget,
        placeholderVariable,
        thisTermToReplaceWith,
        otherTermToReplaceWith)
      substitutionsWithoutOtherSubstitutedStatementVariable =
        copy(unknownSubstitutions = unknownSubstitutions - otherSubstitutedStatementVariable)
      substitutionsWithResolvedStatement <- substitutionsWithoutOtherSubstitutedStatementVariable
        .tryAdd(thisSubstitutedStatementVariable.statementVariable, resolvedStatement)
      substitutionsWithSharedTermToBeReplaced <- substitutionsWithResolvedStatement
        .tryAdd(sharedTermToBeReplaced, placeholderVariable)
    } yield substitutionsWithSharedTermToBeReplaced
  }

  private def tryAddingByCalculatingTermToReplaceWith(
    substitutedStatementVariable: SubstitutedStatementVariable,
    statement: Statement
  ): Option[PartialSubstitutions] = {
    for {
      mappedStatementVariable <- knownStatements.get(substitutedStatementVariable.statementVariable)
      mappedTermToBeReplaced <- knownTerms.get(substitutedStatementVariable.termToBeReplaced)
        .flatMap(Term.optionAsVariable)
      (mappedTermToReplaceWithOption, distinctVariables) <-
        mappedStatementVariable.findSubstitution(statement, mappedTermToBeReplaced).headOption
      mappedTermToReplaceWith <- mappedTermToReplaceWithOption
      s1 <- substitutedStatementVariable.termToReplaceWith.calculateSubstitutions(mappedTermToReplaceWith, this)
      s2 = s1.copy(distinctVariables = s1.distinctVariables ++ distinctVariables)
    } yield s2
  }

  def tryResolve(): Option[(Substitutions, Map[TermVariable, Variables])] = {
    unknownSubstitutions.keys.toList match {
      case Nil =>
        Some(Substitutions(knownStatements, knownTerms), distinctVariables)
      case one +: more =>
        copy(unknownSubstitutions = unknownSubstitutions.filterKeys(more.contains))
          .tryAddDirectly(one, unknownSubstitutions(one))
          .flatMap(_.tryResolve())
    }
  }
}

object PartialSubstitutions {
  val empty: PartialSubstitutions = PartialSubstitutions(Map.empty, Map.empty, Map.empty, Map.empty)
}
