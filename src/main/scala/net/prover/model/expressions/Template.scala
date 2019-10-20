package net.prover.model.expressions

import net.prover.model.expressions
import net.prover.model._
import net.prover.model.entries.{ExpressionDefinition, StatementDefinition, TermDefinition}

sealed trait Template {
  def names: Seq[String]
  def matchExpression(expression: Expression): Option[Seq[Template.Match]] = matchExpression(expression, Nil, Nil)
  protected def matchExpression(expression: Expression, boundVariableNames: Seq[Seq[String]], internalPath: Seq[Int]): Option[Seq[Template.Match]]
  def replaceDefinition(
    oldDefinition: ExpressionDefinition,
    newDefinition: ExpressionDefinition
  ): Template
  def referencedDefinitions: Set[ExpressionDefinition]
  def variables: Seq[Template.Variable]
  def expand(statements: Map[String, Statement], terms: Map[String, Term]): Expression
  def expressionParser(implicit expressionParsingContext: ExpressionParsingContext): Parser[Expression] = {
    variables.distinct.foldLeft(Parser.constant((Map.empty[String, Statement], Map.empty[String, Term]))) {
      case (mapParser, Template.StatementVariable(name)) =>
        for {
          statementMapAndTermMap <- mapParser
          (statementMap, termMap) = statementMapAndTermMap
          newStatement <- Statement.parser
        } yield (statementMap + (name -> newStatement), termMap)
      case (mapParser, Template.TermVariable(name)) =>
        for {
          statementMapAndTermMap <- mapParser
          (statementMap, termMap) = statementMapAndTermMap
          newTerm <- Term.parser
        } yield (statementMap, termMap + (name -> newTerm))
    }.map { case (statementMap, termMap) =>
      expand(statementMap, termMap)
    }
  }
  def serialized: String
}

object Template {
  sealed trait Variable extends Template {
    def name: String
  }
  case class StatementVariable(name: String) extends Template.Variable {
    override def names: Seq[String] = Seq(name)
    override def matchExpression(expression: Expression, boundVariableNames: Seq[Seq[String]], internalPath: Seq[Int]): Option[Seq[Template.Match]] = expression match {
      case statement: Statement => Some(Seq(Template.Match.Component(statement, boundVariableNames, internalPath)))
      case _ => None
    }
    override def replaceDefinition(
      oldDefinition: ExpressionDefinition,
      newDefinition: ExpressionDefinition
    ): Template = this
    override def variables: Seq[Variable] = Seq(this)
    override def expand(statements: Map[String, Statement], terms: Map[String, Term]): Statement = statements(name)
    override def referencedDefinitions: Set[ExpressionDefinition] = Set.empty
    override def serialized: String = name
  }
  case class TermVariable(name: String) extends Template.Variable {
    override def names: Seq[String] = Seq(name)
    override def matchExpression(
      expression: Expression,
      boundVariableNames: Seq[Seq[String]],
      internalPath: Seq[Int]
    ): Option[Seq[Match.Component]] = expression match {
      case term: Term => Some(Seq(Template.Match.Component(term, boundVariableNames, internalPath)))
      case _ => None
    }
    override def replaceDefinition(
      oldDefinition: ExpressionDefinition,
      newDefinition: ExpressionDefinition
    ): Template = this
    override def variables: Seq[Variable] = Seq(this)
    override def expand(statements: Map[String, Statement], terms: Map[String, Term]): Term = terms(name)
    override def referencedDefinitions: Set[ExpressionDefinition] = Set.empty
    override def serialized: String = name
  }
  case class FunctionParameter(parameter: expressions.FunctionParameter) extends Template {
    override def names: Nil.type = Nil
    override def matchExpression(
      expression: Expression,
      boundVariableNames: Seq[Seq[String]],
      internalPath: Seq[Int]
    ): Option[Nil.type] = expression match {
      case expressions.FunctionParameter(parameter.index, parameter.level) => Some(Nil)
      case _ => None
    }
    override def replaceDefinition(
      oldDefinition: ExpressionDefinition,
      newDefinition: ExpressionDefinition
    ): Template = this
    override def variables: Seq[Variable] = Nil
    override def expand(statements: Map[String, Statement], terms: Map[String, Term]): Term = {
      throw new Exception("Parsing templated parameters not currently supported")
    }
    override def referencedDefinitions: Set[ExpressionDefinition] = Set.empty
    override def serialized: String = parameter.toString
  }
  case class DefinedStatement(
      definition: StatementDefinition,
      boundVariableNames: Seq[String],
      components: Seq[Template])
    extends Template
  {
    override def names: Seq[String] = boundVariableNames ++ components.flatMap(_.names)
    override def matchExpression(
      expression: Expression,
      outerBoundVariableNames: Seq[Seq[String]],
      internalPath: Seq[Int]
    ): Option[Seq[Match]] = expression match {
      case definedStatement @ expressions.DefinedStatement(matchedComponents, `definition`) =>
        for {
          pairs <- components.zipStrict(matchedComponents)
          submatches <- pairs.mapWithIndex { case ((a, b), i) => a.matchExpression(b, outerBoundVariableNames :+ definedStatement.scopedBoundVariableNames, internalPath :+ i) }.traverseOption
        } yield definedStatement.scopedBoundVariableNames.mapWithIndex((name, index) => Template.Match.BoundVariable(name, index, internalPath)) ++ submatches.flatten
      case _ =>
        None
    }
    override def replaceDefinition(
      oldDefinition: ExpressionDefinition,
      newDefinition: ExpressionDefinition
    ): Template = {
      if (oldDefinition == definition)
        copy(definition = newDefinition.asInstanceOf[StatementDefinition])
      else
        this
    }
    override def variables: Seq[Variable] = components.flatMap(_.variables)
    override def expand(statements: Map[String, Statement], terms: Map[String, Term]): Statement = {
      if (boundVariableNames.nonEmpty) throw new Exception("Parsing templated statements with bound variables not currently supported")
      definition(components.map(_.expand(statements, terms)):_*)
    }
    override def referencedDefinitions: Set[ExpressionDefinition] = components.flatMap(_.referencedDefinitions).toSet + definition
    override def serialized: String = (Seq(definition.symbol) ++ boundVariableNames ++ components.map(_.serialized)).mkString(" ")
  }
  case class DefinedTerm(
      definition: TermDefinition,
      boundVariableNames: Seq[String],
      components: Seq[Template])
    extends Template
  {
    override def names: Seq[String] = boundVariableNames ++ components.flatMap(_.names)
    override def matchExpression(
      expression: Expression,
      outerBoundVariableNames: Seq[Seq[String]],
      internalPath: Seq[Int]
    ): Option[Seq[Match]] = expression match {
      case definedTerm @ expressions.DefinedTerm(matchedComponents, `definition`) =>
        for {
          pairs <- components.zipStrict(matchedComponents)
          submatches <- pairs.mapWithIndex { case ((a, b), i) => a.matchExpression(b, outerBoundVariableNames :+ definedTerm.scopedBoundVariableNames, internalPath :+ i) }.traverseOption
        } yield definedTerm.scopedBoundVariableNames.mapWithIndex((name, index) => Template.Match.BoundVariable(name, index, internalPath)) ++ submatches.flatten
      case _ =>
        None
    }
    override def replaceDefinition(
      oldDefinition: ExpressionDefinition,
      newDefinition: ExpressionDefinition
    ): Template = {
      if (oldDefinition == definition)
        copy(definition = newDefinition.asInstanceOf[TermDefinition])
      else
        this
    }
    override def referencedDefinitions: Set[ExpressionDefinition] = components.flatMap(_.referencedDefinitions).toSet + definition
    override def serialized: String = (Seq(definition.symbol) ++ boundVariableNames ++ components.map(_.serialized)).mkString(" ")
    override def variables: Seq[Variable] = components.flatMap(_.variables)
    override def expand(statements: Map[String, Statement], terms: Map[String, Term]): Term = {
      if (boundVariableNames.nonEmpty) throw new Exception("Parsing templated terms with bound variables not currently supported")
      definition(components.map(_.expand(statements, terms)):_*)
    }
  }

  sealed trait Match
  object Match {
    case class BoundVariable(name: String, index: Int, internalPath: Seq[Int]) extends Match
    case class Component(expression: Expression, boundVariableNames: Seq[Seq[String]], internalPath: Seq[Int]) extends Match
  }

  def parser(implicit entryContext: EntryContext): Parser[Template] = {
    implicit val templateParsingContext: TemplateParsingContext = TemplateParsingContext(entryContext, Nil)
    Parser.selectWordParser("template")(Statement.templateParserFunction orElse Term.templateParserFunction)
  }
}
