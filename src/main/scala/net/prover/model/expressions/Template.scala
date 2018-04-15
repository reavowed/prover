package net.prover.model.expressions

import net.prover.model.expressions
import net.prover.model.{Parser, ParsingContext}
import net.prover.model.entries.{StatementDefinition, TermDefinition}

sealed trait Template {
  def names: Seq[String]
  def matchExpression(expression: Expression): Option[Seq[Either[String, Expression]]]
}

object Template {
  case class StatementVariable(name: String) extends Template {
    override def names = Seq(name)
    override def matchExpression(expression: Expression) = expression match {
      case statement: Statement => Some(Seq(Right(statement)))
      case _ => None
    }
  }
  case class TermVariable(name: String) extends Template {
    override def names = Seq(name)
    override def matchExpression(expression: Expression) = expression match {
      case term: Term => Some(Seq(Right(term)))
      case _ => None
    }
  }
  case class FunctionParameter(parameter: expressions.FunctionParameter) extends Template {
    override def names = Nil
    override def matchExpression(expression: Expression) = expression match {
      case expressions.FunctionParameter(parameter.index, level, depth) if (depth - level) == (parameter.depth - parameter.level) => Some(Nil)
      case _ => None
    }
  }
  case class DefinedStatement(
      definition: StatementDefinition,
      boundVariableNames: Seq[String],
      components: Seq[Template])
    extends Template
  {
    override def names = boundVariableNames ++ components.flatMap(_.names)
    override def matchExpression(expression: Expression) = expression match {
      case definedStatement @ expressions.DefinedStatement(matchedComponents, `definition`, _) =>
        for {
          pairs <- components.zipStrict(matchedComponents)
          submatches <- pairs.map { case (a, b) => a.matchExpression(b) }.traverseOption
        } yield definedStatement.scopedBoundVariableNames.map(Left(_)) ++ submatches.flatten
      case _ =>
        None
    }
  }
  case class DefinedTerm(
      definition: TermDefinition,
      boundVariableNames: Seq[String],
      components: Seq[Template])
    extends Template
  {
    override def names = boundVariableNames ++ components.flatMap(_.names)
    override def matchExpression(expression: Expression) = expression match {
      case definedTerm @ expressions.DefinedTerm(matchedComponents, `definition`, _) =>
        for {
          pairs <- components.zipStrict(matchedComponents)
          submatches <- pairs.map { case (a, b) => a.matchExpression(b) }.traverseOption
        } yield definedTerm.scopedBoundVariableNames.map(Left(_)) ++ submatches.flatten
      case _ =>
        None
    }
  }

  def parser(implicit context: ParsingContext): Parser[Template] = {
    Statement.templateParser.tryOrElse(Term.templateParser)
  }
}