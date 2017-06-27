package net.prover.model

case class Conditions(arbitraryVariables: Set[TermVariable], distinctVariables: DistinctVariables) {
  def ++(other: Conditions): Conditions = {
    Conditions(
      arbitraryVariables ++ other.arbitraryVariables,
      distinctVariables ++ other.distinctVariables)
  }

  def addDistinctVariables(newDistinctVariables: DistinctVariables): Conditions = {
    copy(distinctVariables = distinctVariables ++ newDistinctVariables)
  }

  def addDistinctVariables(termVariables: Set[TermVariable], statements: Seq[Statement]): Conditions = {
    addDistinctVariables(DistinctVariables.byStatements(termVariables, statements))
  }

  def restrictToStatements(statements: Seq[Statement]): Conditions = {
    val activeVariables = statements.map(_.allVariables).foldTogether
    val updatedDistinctVariables = distinctVariables.restrictTo(activeVariables)
    val updatedArbitraryVariables = arbitraryVariables
      .intersect(activeVariables.termVariables)
      .filter { v =>
        statements.exists { s =>
          def intersectsNonArbitrary: Boolean = {
            val intersectingVariables = s.getPotentiallyIntersectingVariables(v)
            intersectingVariables.statementVariables.nonEmpty ||
              !intersectingVariables.termVariables.forall(arbitraryVariables.contains)
          }
          !s.boundVariables.contains(v) && intersectsNonArbitrary
        }
      }
    Conditions(updatedArbitraryVariables, updatedDistinctVariables)
  }

  def removeImplicitDistinctVariables(implicitDistinctVariables: DistinctVariables): Conditions = {
    copy(distinctVariables = distinctVariables -- implicitDistinctVariables)
  }

  def applySubstitutions(
    substitutions: Substitutions
  ): Option[Conditions] = {
    for {
      updatedArbitraryVariables <- arbitraryVariables
        .map(_.applySubstitutions(substitutions).flatMap(Term.optionAsVariable))
        .traverseOption
      updatedDistinctVariables <- distinctVariables.applySubstitutions(substitutions)
    } yield {
      Conditions(updatedArbitraryVariables, updatedDistinctVariables)
    }
  }

  def isEmpty: Boolean = {
    arbitraryVariables.isEmpty && distinctVariables.conditions.isEmpty
  }
}

object Conditions {
  val empty = Conditions(Set.empty, DistinctVariables.empty)

  def arbitraryVariablesParser(implicit context: Context): Parser[Option[Set[TermVariable]]] = {
    Parser.optional(
      "arbitrary-variables",
      Term.variableListParser.map(_.toSet))
  }

  def variablePairParser(implicit context: Context): Parser[(TermVariable, Variable)] = {
    for {
      first <- Term.variableParser
      second <- Component.variableParser
    } yield first -> second
  }

  def variablePairListParser(implicit context: Context): Parser[Seq[(TermVariable, Variable)]] = {
    variablePairParser.listInParens(Some(","))
  }

  def optionalDistinctVariablesParser(implicit context: Context): Parser[Option[DistinctVariables]] = {
    Parser.optional(
      "distinct-variables",
      variablePairListParser.map(DistinctVariables(_: _*)))
  }

  def distinctVariablesParser(implicit context: Context): Parser[DistinctVariables] = {
    optionalDistinctVariablesParser.getOrElse(DistinctVariables.empty)
  }

  def optionalParser(implicit context: Context): Parser[Option[Conditions]] = {
    for {
      arbitraryVariablesOption <- arbitraryVariablesParser
      distinctVariablesOption <- optionalDistinctVariablesParser
    } yield {
      if (arbitraryVariablesOption.isEmpty && distinctVariablesOption.isEmpty)
        None
      else
        Some(Conditions(
          arbitraryVariablesOption.getOrElse(Set.empty),
          distinctVariablesOption.getOrElse(DistinctVariables.empty)))
    }
  }

  def parser(implicit context: Context): Parser[Conditions] = {
    optionalParser.getOrElse(Conditions.empty)
  }
}
