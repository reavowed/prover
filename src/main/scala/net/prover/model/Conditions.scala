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
    val activeVariables = statements.flatMap(_.allVariables).toSet
    val updatedDistinctVariables = distinctVariables.restrictTo(statements)
    val updatedArbitraryVariables = arbitraryVariables
      .intersect(activeVariables.ofType[TermVariable])
      .filter { arbitraryVariable =>
        statements.exists { statement =>
          (statement.presentVariables.contains(arbitraryVariable) ||
            statement.getPotentiallyIntersectingVariables(arbitraryVariable)
              .ofType[StatementVariable]
              .exists { statementVariable => !distinctVariables.areDistinct(arbitraryVariable, statementVariable) }) &&
            !statement.boundVariables.contains(arbitraryVariable)
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

  def serialized: String = {
    val arbitrary =
      if (arbitraryVariables.nonEmpty)
        Some(s"arbitrary-variables (${arbitraryVariables.map(_.text).mkString(", ")})")
      else
        None
    val distinct =
      if (distinctVariables.nonEmpty)
        Some(s"distinct-variables (${distinctVariables.serialized})")
      else None
    (arbitrary.toSeq ++ distinct.toSeq).mkString(" ")
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
      second <- Variable.parser
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

  implicit class ConditionsSeqOps(seq: Traversable[Conditions]) {
    def foldTogether: Conditions = {
      seq.foldLeft(Conditions.empty)(_ ++ _)
    }
  }
}
