package net.prover.model

case class Substitutions(
    statements: Map[StatementVariable, Statement],
    terms: Map[TermVariable, Term])

case class PartialSubstitutions(
    knownStatements: Map[StatementVariable, Statement],
    knownTerms: Map[TermVariable, Term],
    unknownSubstitutions: Map[SubstitutedStatementVariable, Statement],
    distinctVariables: Map[TermVariable, Variables]) {

  def knownSubstitutions = Substitutions(knownStatements, knownTerms)

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
  ): Seq[PartialSubstitutions] = {
    val directAttempts = tryAddingDirectly(substitutedStatementVariable, statement)
    if (directAttempts.nonEmpty)
      directAttempts
    else
      Seq(copy(unknownSubstitutions = unknownSubstitutions + (substitutedStatementVariable -> statement)))
  }

  private def tryAddingDirectly(
    substitutedStatementVariable: SubstitutedStatementVariable,
    targetStatement: Statement
  ): Seq[PartialSubstitutions] = {
    val substitutedBaseStatementOption = knownStatements.get(substitutedStatementVariable.statementVariable)
    val substitutedTermToReplaceWithOption = substitutedStatementVariable.termToReplaceWith.applySubstitutions(knownSubstitutions, distinctVariables)
    val substitutedTermToBeReplacedOption = knownTerms.get(substitutedStatementVariable.termToBeReplaced).flatMap(Term.optionAsVariable)

    (substitutedBaseStatementOption, substitutedTermToReplaceWithOption, substitutedTermToBeReplacedOption) match {
      case (Some(substitutedBaseStatement), Some(substitutedTermToReplaceWith), Some(substitutedTermToBeReplaced)) =>
        tryNotAddingAtAll(substitutedBaseStatement, substitutedTermToReplaceWith, substitutedTermToBeReplaced, targetStatement)
      case (Some(substitutedBaseStatement), None, Some(substitutedTermToBeReplaced)) =>
        tryAddingByCalculatingTermToReplaceWith(substitutedBaseStatement, substitutedTermToBeReplaced, substitutedStatementVariable.termToReplaceWith, targetStatement)
      case _ =>
        findMergableSubstitution(substitutedStatementVariable) match {
          case Some(otherSubstitutedStatementVariable) =>
            tryAddingByMerge(substitutedStatementVariable, targetStatement, otherSubstitutedStatementVariable).toSeq
          case _ =>
            Nil
        }
    }
  }

  private def tryNotAddingAtAll(
    substitutedBaseStatement: Statement,
    substitutedTermToReplaceWith: Term,
    substitutedTermToBeReplaced: TermVariable,
    targetStatement: Statement
  ): Seq[PartialSubstitutions] = {
    if (substitutedBaseStatement.makeSingleSubstitution(substitutedTermToReplaceWith, substitutedTermToBeReplaced, distinctVariables).contains(targetStatement))
      Seq(this)
    else
      Nil
  }

  private def tryAddingByCalculatingTermToReplaceWith(
    substitutedBaseStatement: Statement,
    substitutedTermToBeReplaced: TermVariable,
    termToReplaceWith: Term,
    targetStatement: Statement
  ): Seq[PartialSubstitutions] = {
    for {
      (substitutedTermToReplaceWithOption, newDistinctVariables) <- substitutedBaseStatement.findSubstitution(targetStatement, substitutedTermToBeReplaced)
      substitutedTermToReplaceWith <- substitutedTermToReplaceWithOption.toSeq
      updatedSubstitutions <- termToReplaceWith.calculateSubstitutions(substitutedTermToReplaceWith, this)
    } yield {
      updatedSubstitutions.withDistinctVariables(newDistinctVariables)
    }
  }

  private def findMergableSubstitution(substitutedStatementVariable: SubstitutedStatementVariable): Option[SubstitutedStatementVariable] = {
    unknownSubstitutions.keySet
      .find { s =>
        s.statementVariable == substitutedStatementVariable.statementVariable &&
          s.termToBeReplaced == substitutedStatementVariable.termToBeReplaced
      }
  }

  private def tryAddingByMerge(
    newSubstitutedStatementVariable: SubstitutedStatementVariable,
    newTargetStatement: Statement,
    otherSubstitutedStatementVariable: SubstitutedStatementVariable
  ): Option[PartialSubstitutions] = {
    val otherTargetStatement = unknownSubstitutions(otherSubstitutedStatementVariable)
    val sharedTermToBeReplaced = otherSubstitutedStatementVariable.termToBeReplaced
    for {
      thisTermToReplaceWith <- newSubstitutedStatementVariable.termToReplaceWith.applySubstitutions(knownSubstitutions, distinctVariables)
      otherTermToReplaceWith <- otherSubstitutedStatementVariable.termToReplaceWith.applySubstitutions(knownSubstitutions, distinctVariables)
      placeholderVariableTerm = knownTerms.getOrElse(
        sharedTermToBeReplaced,
        sharedTermToBeReplaced.copy(text = sharedTermToBeReplaced + "'"))
      placeholderVariable <- Term.optionAsVariable(placeholderVariableTerm)
      resolvedStatement <- newTargetStatement.resolveSingleSubstitution(
        otherTargetStatement,
        placeholderVariable,
        thisTermToReplaceWith,
        otherTermToReplaceWith)
      substitutionsWithoutOtherSubstitutedStatementVariable =
        copy(unknownSubstitutions = unknownSubstitutions - otherSubstitutedStatementVariable)
      substitutionsWithResolvedStatement <- substitutionsWithoutOtherSubstitutedStatementVariable
        .tryAdd(newSubstitutedStatementVariable.statementVariable, resolvedStatement)
      substitutionsWithSharedTermToBeReplaced <- substitutionsWithResolvedStatement
        .tryAdd(sharedTermToBeReplaced, placeholderVariable)
    } yield substitutionsWithSharedTermToBeReplaced
  }

  def withDistinctVariables(additionalDistinctVariables: Map[TermVariable, Variables]): PartialSubstitutions = {
    copy(distinctVariables = distinctVariables.merge(additionalDistinctVariables))
  }

  def tryResolve(): Seq[(Substitutions, Map[TermVariable, Variables])] = {
    unknownSubstitutions.keys.toList match {
      case Nil =>
        Seq((Substitutions(knownStatements, knownTerms), distinctVariables))
      case one +: more =>
        copy(unknownSubstitutions = unknownSubstitutions.filterKeys(more.contains))
          .tryAddingDirectly(one, unknownSubstitutions(one))
          .flatMap(_.tryResolve())
    }
  }
}

object PartialSubstitutions {
  val empty: PartialSubstitutions = PartialSubstitutions(Map.empty, Map.empty, Map.empty, Map.empty)
}
