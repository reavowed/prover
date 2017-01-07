package net.prover.model

case class Match(statements: Map[StatementVariable, Statement], terms: Map[TermVariable, Term]) {
  def -(termVariable: TermVariable): Match = {
    copy(terms = terms + (termVariable -> termVariable))
  }

  def expand(
    requiredVariables: Variables,
    line: PartialLine,
    context: Context
  ): (Match, PartialLine) = {
    val missingStatementVariables = requiredVariables.statementVariables.diff(statements.keySet.toSeq)
    val (missingStatements, lineAfterStatements) = missingStatementVariables.mapFold(line) { (statementVariable, lineSoFar) =>
      Statement.parse(lineSoFar, context).mapLeft(statementVariable -> _)
    }.mapLeft(_.toMap)
    val missingTermVariables = requiredVariables.termVariables.diff(terms.keySet.toSeq)
    val (missingTerms, lineAfterTerms) = missingTermVariables.mapFold(lineAfterStatements) { (termVariable, lineSoFar) =>
      Term.parse(lineSoFar, context).mapLeft(termVariable -> _)
    }.mapLeft(_.toMap)
    (copy(statements = statements ++ missingStatements, terms = terms ++ missingTerms), lineAfterTerms)
  }
}

object Match {
  val empty: Match = Match(Map.empty, Map.empty)
  def mergeMaps[S,T](maps: Seq[Map[S, T]]): Option[Map[S, T]] = {
    val keys = maps.map(_.keySet).fold(Set.empty)(_ ++ _).toSeq
    keys.map(i => maps.flatMap(_.get(i)).distinct match {
      case Seq(singleValue) => Some(i -> singleValue)
      case _ => None
    }).traverseOption.map(_.toMap)
  }
  def mergeAttempts(matchAttempts: Seq[Option[Match]]): Option[Match] = {
    matchAttempts.traverseOption.flatMap(merge)
  }
  def merge(matches: Seq[Match]): Option[Match] = {
    for {
      mergedStatements <- mergeMaps(matches.map(_.statements))
      mergedTerms <- mergeMaps(matches.map(_.terms))
    } yield Match(mergedStatements, mergedTerms)
  }
}

case class MatchWithSubstitutions(
    statements: Map[StatementVariable, Statement],
    terms: Map[TermVariable, Term],
    substitutions: Seq[(StatementVariableWithReplacement, Statement)]) {

  def ++(otherMatch: MatchWithSubstitutions): Option[MatchWithSubstitutions] = MatchWithSubstitutions.merge(Seq(this, otherMatch))

  def expand(
    requiredVariables: Variables,
    line: PartialLine,
    context: Context
  ): (MatchWithSubstitutions, PartialLine) = {
    val missingStatementVariables = requiredVariables.statementVariables.diff(statements.keySet.toSeq)
    val (missingStatements, lineAfterStatements) = missingStatementVariables.mapFold(line) { (statementVariable, lineSoFar) =>
      Statement.parse(lineSoFar, context).mapLeft(statementVariable -> _)
    }.mapLeft(_.toMap)
    val missingTermVariables = requiredVariables.termVariables.diff(terms.keySet.toSeq)
    val (missingTerms, lineAfterTerms) = missingTermVariables.mapFold(lineAfterStatements) { (termVariable, lineSoFar) =>
      Term.parse(lineSoFar, context).mapLeft(termVariable -> _)
    }.mapLeft(_.toMap)
    (copy(statements = statements ++ missingStatements, terms = terms ++ missingTerms), lineAfterTerms)
  }

  def checkSubstitutions(distinctVariables: DistinctVariables): Option[Match] = {
    substitutions.foldLeft(Option(Match(statements, terms))) {
      case (matchOption, (substitutionTemplate, statement)) =>
        matchOption.filter(m => substitutionTemplate.applyMatch(m, distinctVariables) == statement)
    }
  }
}

object MatchWithSubstitutions {
  val empty: MatchWithSubstitutions = MatchWithSubstitutions(Map.empty, Map.empty, Nil)
  import Match.mergeMaps
  def mergeAttempts(matchAttempts: Seq[Option[MatchWithSubstitutions]]): Option[MatchWithSubstitutions] = {
    matchAttempts.traverseOption.flatMap(merge)
  }
  def merge(matches: Seq[MatchWithSubstitutions]): Option[MatchWithSubstitutions] = {
    for {
      mergedStatements <- mergeMaps(matches.map(_.statements))
      mergedTerms <- mergeMaps(matches.map(_.terms))
    } yield MatchWithSubstitutions(mergedStatements, mergedTerms, matches.flatMap(_.substitutions))
  }
}
