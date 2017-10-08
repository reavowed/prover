package net.prover.model

import net.prover.model.expressions._

case class Substitutions(
  expressionsByVariable: Map[Variable, Expression],
  predicatesByName: Map[PredicateVariable, Predicate])
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

  def addPredicate(name: PredicateVariable, predicate: Predicate): Option[Substitutions] = {
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

//  case class PredicateDetails(predicate: Predicate, arity: Int) {
//    def serialized: String = s"$arity ${predicate.serialized}"
//  }
//  object PredicateDetails {
//    def parser(implicit parsingContext: ParsingContext): Parser[PredicateDetails] = {
//      for {
//        arity <- Parser.int
//        predicate <- Predicate.parser(parsingContext.addParameterList((0 until arity).map(i => s"$$$i")))
//      } yield PredicateDetails(predicate, arity)
//    }
//  }

  case class Required(variables: Seq[Variable], predicates: Seq[PredicateVariable]) {
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
