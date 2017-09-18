package net.prover.model

import net.prover.model.components._

case class Substitutions(
  componentsByVariable: Map[Variable, Component],
  predicatesByName: Map[String, Predicate])
{
  def addVariable(variable: Variable, component: Component): Option[Substitutions] = {
    componentsByVariable.get(variable) match {
      case Some(`component`) =>
        Some(this)
      case Some(_) =>
        None
      case None =>
        Some(copy(componentsByVariable = componentsByVariable.updated(variable, component)))
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
