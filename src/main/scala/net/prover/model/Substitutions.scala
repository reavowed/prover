package net.prover.model

import net.prover.model.expressions._

case class Substitutions(
  expressionsByVariable: Map[Variable, Expression],
  predicatesByName: Map[String, Predicate])
{
  def addVariable(variable: Variable, expression: Expression): Option[Substitutions] = {
    expressionsByVariable.get(variable) match {
      case Some(`expression`) =>
        Some(this)
      case Some(_) =>
        None
      case None =>
        Some(copy(expressionsByVariable = expressionsByVariable.updated(variable, expression)))
    }
  }

  def addPredicate(name: String, predicate: Predicate): Option[Substitutions] = {
    predicatesByName.get(name) match {
      case Some(`predicate`) =>
        Some(this)
      case Some(_) =>
        None
      case None =>
        Some(copy(predicatesByName = predicatesByName.updated(name, predicate)))
    }
  }
}

object Substitutions {
  val empty = Substitutions(Map.empty, Map.empty)

  case class Required(variables: Seq[Variable], predicates: Seq[String]) {
    def ++(other: Required): Required = {
      Required(
        (variables ++ other.variables).distinct,
        (predicates ++ other.predicates).distinct)
    }
  }

  object Required {
    val empty = Required(Seq.empty, Seq.empty)
    implicit class RequiredSeqOps(seq: Seq[Required]) {
      def foldTogether: Required = {
        Required(
          seq.flatMap(_.variables).distinct,
          seq.flatMap(_.predicates).distinct)
      }
    }
  }
}
