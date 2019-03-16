package net.prover.model.expressions

import net.prover.model.expressions
import net.prover.model.{Parser, ParsingContext}
import net.prover.model.entries.{StatementDefinition, TermDefinition}

sealed trait Template {
  def names: Seq[String]
  def matchExpression(expression: Expression, boundVariableNames: Seq[Seq[String]]): Option[Seq[Template.Match]]
  def serialized: String
}

object Template {
  case class StatementVariable(name: String) extends Template {
    override def names = Seq(name)
    override def matchExpression(expression: Expression, boundVariableNames: Seq[Seq[String]]): Option[Seq[Template.Match]] = expression match {
      case statement: Statement => Some(Seq(Template.Match.Component(statement, boundVariableNames)))
      case _ => None
    }
    override def serialized = name
  }
  case class TermVariable(name: String) extends Template {
    override def names = Seq(name)
    override def matchExpression(expression: Expression, boundVariableNames: Seq[Seq[String]]) = expression match {
      case term: Term => Some(Seq(Template.Match.Component(term, boundVariableNames)))
      case _ => None
    }
    override def serialized = name
  }
  case class FunctionParameter(parameter: expressions.FunctionParameter) extends Template {
    override def names = Nil
    override def matchExpression(expression: Expression, boundVariableNames: Seq[Seq[String]]) = expression match {
      case expressions.FunctionParameter(parameter.index, parameter.level) => Some(Nil)
      case _ => None
    }
    override def serialized = parameter.toString
  }
  case class DefinedStatement(
      definition: StatementDefinition,
      boundVariableNames: Seq[String],
      components: Seq[Template])
    extends Template
  {
    override def names = boundVariableNames ++ components.flatMap(_.names)
    override def matchExpression(expression: Expression, outerBoundVariableNames: Seq[Seq[String]]) = expression match {
      case definedStatement @ expressions.DefinedStatement(matchedComponents, `definition`) =>
        for {
          pairs <- components.zipStrict(matchedComponents)
          submatches <- pairs.map { case (a, b) => a.matchExpression(b, outerBoundVariableNames :+ definedStatement.scopedBoundVariableNames) }.traverseOption
        } yield definedStatement.scopedBoundVariableNames.map(Template.Match.BoundVariable) ++ submatches.flatten
      case _ =>
        None
    }
    override def serialized = (Seq(definition.symbol) ++ boundVariableNames ++ components.map(_.serialized)).mkString(" ")
  }
  case class DefinedTerm(
      definition: TermDefinition,
      boundVariableNames: Seq[String],
      components: Seq[Template])
    extends Template
  {
    override def names = boundVariableNames ++ components.flatMap(_.names)
    override def matchExpression(expression: Expression, outerBoundVariableNames: Seq[Seq[String]]) = expression match {
      case definedTerm @ expressions.DefinedTerm(matchedComponents, `definition`) =>
        for {
          pairs <- components.zipStrict(matchedComponents)
          submatches <- pairs.map { case (a, b) => a.matchExpression(b, outerBoundVariableNames :+ definedTerm.scopedBoundVariableNames) }.traverseOption
        } yield definedTerm.scopedBoundVariableNames.map(Template.Match.BoundVariable) ++ submatches.flatten
      case _ =>
        None
    }
    override def serialized = (Seq(definition.symbol) ++ boundVariableNames ++ components.map(_.serialized)).mkString(" ")
  }

  sealed trait Match
  object Match {
    case class BoundVariable(name: String) extends Match
    case class Component(expression: Expression, boundVariableNames: Seq[Seq[String]]) extends Match
  }

  def parser(implicit context: ParsingContext): Parser[Template] = {
    Statement.templateParser.tryOrElse(Term.templateParser)
  }
}
