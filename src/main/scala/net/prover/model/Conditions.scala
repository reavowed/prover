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

  def addDistinctVariables(termVariables: Set[TermVariable], statements: Seq[Statement]): Option[Conditions] = {
    DistinctVariables.byStatements(termVariables, statements).map(addDistinctVariables)
  }

  def restrictToStatements(statements: Seq[Statement]): Conditions = {
    val activeVariables = statements.map(_.allVariables).reduce(_ ++ _)
    val updatedDistinctVariables = distinctVariables.restrictTo(activeVariables)
    val updatedArbitraryVariables = arbitraryVariables
      .intersect(activeVariables.termVariables)
      .filter { v =>
        val threats = statements.map(_.getPotentiallyIntersectingVariables(v)).reduce(_ ++ _)
        val protectors = distinctVariables.get(v)
        !(threats.statementVariables.forall(w => protectors.statementVariables.contains(w)) &&
          threats.termVariables.forall(w => protectors.termVariables.contains(w) || distinctVariables.get(w).termVariables.contains(v)))
      }
    Conditions(updatedArbitraryVariables, updatedDistinctVariables)
  }

  def filterOutBoundVariables(boundVariables: Set[TermVariable]): Conditions = {
    copy(arbitraryVariables = arbitraryVariables.diff(boundVariables))
  }

  def restrictToActiveVariables(activeVariables: Variables): Conditions = {
    copy(
      arbitraryVariables = arbitraryVariables.intersect(activeVariables.termVariables),
      distinctVariables = distinctVariables.restrictTo(activeVariables))
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
}

object Conditions {
  val empty = Conditions(Set.empty, DistinctVariables.empty)

  def arbitraryVariablesParser(implicit context: Context): Parser[Option[Set[TermVariable]]] = {
    Parser.optional(
      "arbitrary-variables",
      Term.variableListParser.map(_.toSet).map(Some.apply),
      None)
  }

  def variablePairParser(implicit context: Context): Parser[(Variable, Variable)] = {
    for {
      first <- Component.variableParser
      second <- Component.variableParser
    } yield first -> second
  }

  def variablePairListParser(implicit context: Context): Parser[Seq[(Variable, Variable)]] = {
    variablePairParser.listInParens(Some(","))
  }

  def optionalDistinctVariablesParser(implicit context: Context): Parser[Option[DistinctVariables]] = {
    Parser.optional(
      "distinct-variables",
      variablePairListParser.map(DistinctVariables(_: _*)).map(Some.apply),
      None)
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
