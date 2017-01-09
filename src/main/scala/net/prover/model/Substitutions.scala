package net.prover.model

case class Substitutions(
    statements: Map[StatementVariable, Statement],
    terms: Map[TermVariable, Term]) {
  def -(termVariable: TermVariable): Substitutions = {
    copy(terms = terms + (termVariable -> termVariable))
  }

  def expand(
    requiredVariables: Variables,
    line: PartialLine,
    context: Context
  ): (Substitutions, PartialLine) = {
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

object Substitutions {
  val empty: Substitutions = Substitutions(Map.empty, Map.empty)
  private def mergeMaps[S,T](maps: Seq[Map[S, T]]): Option[Map[S, T]] = {
    val keys = maps.map(_.keySet).fold(Set.empty)(_ ++ _).toSeq
    keys.map(i => maps.flatMap(_.get(i)).distinct match {
      case Seq(singleValue) => Some(i -> singleValue)
      case _ => None
    }).traverseOption.map(_.toMap)
  }
  def mergeAttempts(substitutionAttempts: Seq[Option[Substitutions]]): Option[Substitutions] = {
    substitutionAttempts.traverseOption.flatMap(merge)
  }
  def merge(substitutions: Seq[Substitutions]): Option[Substitutions] = {
    for {
      mergedStatements <- mergeMaps(substitutions.map(_.statements))
      mergedTerms <- mergeMaps(substitutions.map(_.terms))
    } yield Substitutions(mergedStatements, mergedTerms)
  }
}
