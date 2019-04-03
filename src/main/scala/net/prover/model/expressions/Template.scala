package net.prover.model.expressions

import net.prover.model.expressions
import net.prover.model._
import net.prover.model.entries.{StatementDefinition, TermDefinition}

sealed trait Template {
  def names: Seq[String]
  def matchExpression(expression: Expression): Option[Seq[Template.Match]] = matchExpression(expression, Nil, Nil)
  protected def matchExpression(expression: Expression, boundVariableNames: Seq[Seq[String]], internalPath: Seq[Int]): Option[Seq[Template.Match]]
  def serialized: String
}

object Template {
  case class StatementVariable(name: String) extends Template {
    override def names = Seq(name)
    override def matchExpression(expression: Expression, boundVariableNames: Seq[Seq[String]], internalPath: Seq[Int]): Option[Seq[Template.Match]] = expression match {
      case statement: Statement => Some(Seq(Template.Match.Component(statement, boundVariableNames, internalPath)))
      case _ => None
    }
    override def serialized = name
  }
  case class TermVariable(name: String) extends Template {
    override def names = Seq(name)
    override def matchExpression(expression: Expression, boundVariableNames: Seq[Seq[String]], internalPath: Seq[Int]) = expression match {
      case term: Term => Some(Seq(Template.Match.Component(term, boundVariableNames, internalPath)))
      case _ => None
    }
    override def serialized = name
  }
  case class FunctionParameter(parameter: expressions.FunctionParameter) extends Template {
    override def names = Nil
    override def matchExpression(expression: Expression, boundVariableNames: Seq[Seq[String]], internalPath: Seq[Int]) = expression match {
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
    override def matchExpression(expression: Expression, outerBoundVariableNames: Seq[Seq[String]], internalPath: Seq[Int]) = expression match {
      case definedStatement @ expressions.DefinedStatement(matchedComponents, `definition`) =>
        for {
          pairs <- components.zipStrict(matchedComponents)
          submatches <- pairs.mapWithIndex { case ((a, b), i) => a.matchExpression(b, outerBoundVariableNames :+ definedStatement.scopedBoundVariableNames, internalPath :+ i) }.traverseOption
        } yield definedStatement.scopedBoundVariableNames.mapWithIndex((name, index) => Template.Match.BoundVariable(name, index, internalPath)) ++ submatches.flatten
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
    override def matchExpression(expression: Expression, outerBoundVariableNames: Seq[Seq[String]], internalPath: Seq[Int]) = expression match {
      case definedTerm @ expressions.DefinedTerm(matchedComponents, `definition`) =>
        for {
          pairs <- components.zipStrict(matchedComponents)
          submatches <- pairs.mapWithIndex { case ((a, b), i) => a.matchExpression(b, outerBoundVariableNames :+ definedTerm.scopedBoundVariableNames, internalPath :+ i) }.traverseOption
        } yield definedTerm.scopedBoundVariableNames.mapWithIndex((name, index) => Template.Match.BoundVariable(name, index, internalPath)) ++ submatches.flatten
      case _ =>
        None
    }
    override def serialized = (Seq(definition.symbol) ++ boundVariableNames ++ components.map(_.serialized)).mkString(" ")
  }

  sealed trait Match
  object Match {
    case class BoundVariable(name: String, index: Int, internalPath: Seq[Int]) extends Match
    case class Component(expression: Expression, boundVariableNames: Seq[Seq[String]], internalPath: Seq[Int]) extends Match
  }

  def parser(implicit context: ExpressionParsingContext): Parser[Template] = {
    Statement.templateParser.tryOrElse(Term.templateParser)
  }
}
