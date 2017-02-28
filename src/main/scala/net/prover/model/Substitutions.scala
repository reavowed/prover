package net.prover.model

case class Substitutions(
    statements: Map[StatementVariable, Statement],
    terms: Map[TermVariable, Term]) {
  def -(termVariable: TermVariable): Substitutions = {
    copy(terms = terms + (termVariable -> termVariable))
  }

  def expandParser(
    requiredVariables: Variables,
    context: Context
  ): Parser[Substitutions] = {
    val missingStatementVariables = requiredVariables.statementVariables.diff(statements.keySet.toSeq)
    val missingTermVariables = requiredVariables.termVariables.diff(terms.keySet.toSeq)
    val parserForStatements = missingStatementVariables.map(_ -> Statement.parser(context)).traverseParserMap
    val parserForTerms = missingTermVariables.map(_ -> Term.parser(context)).traverseParserMap
    for {
      statementMap <- parserForStatements
      termMap <- parserForTerms
    } yield {
      copy(statements = statements ++ statementMap, terms = terms ++ termMap)
    }
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
