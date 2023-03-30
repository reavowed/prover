package net.prover.model.expressions

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model._
import net.prover.model.definitions.{ExpressionDefinition, StatementDefinition, TermDefinition}
import net.prover.model.expressions.Template.Match

@JsonSerialize(using = classOf[TemplateSerializer])
sealed trait Template {
  def names: Seq[String]
  def matchExpression(expression: Expression): Option[Seq[Template.Match]] = matchExpression(expression, Nil, Nil)
  def matchExpression(expression: Expression, boundVariableNames: Seq[Seq[String]], internalPath: Seq[Int]): Option[Seq[Template.Match]]
  def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): Template
  def referencedDefinitions: Set[ExpressionDefinition]
  def variables: Seq[VariableTemplate]
  def expand(statements: Map[String, Statement], terms: Map[String, Term]): Expression
  def specify(arguments: Seq[TermTemplate]): Template = specify(arguments, 0)
  def specify(arguments: Seq[TermTemplate], internalDepth: Int): Template
  def expressionParser(implicit expressionParsingContext: ExpressionParsingContext): Parser[Expression] = expressionParserBuilder(Map.empty, Map.empty).map(_._1)
  def expressionParserBuilder(statements: Map[String, Statement], terms: Map[String, Term])(implicit expressionParsingContext: ExpressionParsingContext): Parser[(Expression, Map[String, Statement], Map[String, Term])]
  def serialized: String
}

sealed trait StatementTemplate extends Template
sealed trait TermTemplate extends Template

sealed trait VariableTemplate extends Template {
  def name: String
  override def specify(arguments: Seq[TermTemplate], internalDepth: Int): Template = this
}
sealed trait DefinedExpressionTemplate extends Template {
  def components: Seq[Template]
  def boundVariableNames: Seq[String]
  def withComponents(newComponents: Seq[Template]): Template
  override def specify(arguments: Seq[TermTemplate], internalDepth: Int): Template = {
    withComponents(components.map(c => c.specify(arguments, if (boundVariableNames.nonEmpty) internalDepth + 1 else internalDepth)))
  }
}

case class StatementVariableTemplate(name: String) extends StatementTemplate with VariableTemplate {
  override def names: Seq[String] = Seq(name)
  override def matchExpression(expression: Expression, boundVariableNames: Seq[Seq[String]], internalPath: Seq[Int]): Option[Seq[Template.Match]] = expression match {
    case statement: Statement => Some(Seq(Template.Match.Component(statement, boundVariableNames, internalPath)))
    case _ => None
  }
  override def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): Template = this
  override def variables: Seq[VariableTemplate] = Seq(this)
  override def expand(statements: Map[String, Statement], terms: Map[String, Term]): Statement = statements(name)
  def expressionParserBuilder(statements: Map[String, Statement], terms: Map[String, Term])(implicit expressionParsingContext: ExpressionParsingContext): Parser[(Statement, Map[String, Statement], Map[String, Term])] = {
    statements.get(name) match {
      case Some(statement) =>
        Parser.constant((statement, statements, terms))
      case None =>
        Statement.parser.map { statement =>
          (statement, statements.updated(name, statement), terms)
        }
    }
  }
  override def referencedDefinitions: Set[ExpressionDefinition] = Set.empty
  override def serialized: String = name
}

case class TermVariableTemplate(name: String) extends TermTemplate with VariableTemplate {
  override def names: Seq[String] = Seq(name)
  override def matchExpression(
    expression: Expression,
    boundVariableNames: Seq[Seq[String]],
    internalPath: Seq[Int]
  ): Option[Seq[Match.Component]] = expression match {
    case term: Term => Some(Seq(Template.Match.Component(term, boundVariableNames, internalPath)))
    case _ => None
  }
  override def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): Template = this
  override def variables: Seq[VariableTemplate] = Seq(this)
  override def expand(statements: Map[String, Statement], terms: Map[String, Term]): Term = terms(name)
  def expressionParserBuilder(statements: Map[String, Statement], terms: Map[String, Term])(implicit expressionParsingContext: ExpressionParsingContext): Parser[(Term, Map[String, Statement], Map[String, Term])] = {
    terms.get(name) match {
      case Some(term) =>
        Parser.constant((term, statements, terms))
      case None =>
        Term.parser.map { term =>
          (term, statements, terms.updated(name, term))
        }
    }
  }
  override def referencedDefinitions: Set[ExpressionDefinition] = Set.empty
  override def serialized: String = name
}

case class FunctionParameterTemplate(parameter: FunctionParameter) extends TermTemplate {
  override def names: Nil.type = Nil
  override def matchExpression(
    expression: Expression,
    boundVariableNames: Seq[Seq[String]],
    internalPath: Seq[Int]
  ): Option[Nil.type] = expression match {
    case FunctionParameter(parameter.index, parameter.level) => Some(Nil)
    case _ => None
  }
  override def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): Template = this
  override def variables: Seq[VariableTemplate] = Nil
  override def expand(statements: Map[String, Statement], terms: Map[String, Term]): Term = {
    parameter
  }
  override def specify(arguments: Seq[TermTemplate], internalDepth: Int): Template = {
    if (internalDepth == parameter.level) {
      arguments(parameter.index)
    } else {
      this
    }
  }
  def expressionParserBuilder(statements: Map[String, Statement], terms: Map[String, Term])(implicit expressionParsingContext: ExpressionParsingContext): Parser[(FunctionParameter, Map[String, Statement], Map[String, Term])] = {
    Parser.constant((parameter, statements, terms))
  }
  override def referencedDefinitions: Set[ExpressionDefinition] = Set.empty
  override def serialized: String = parameter.toString
}
case class DefinedStatementTemplate(
  definition: StatementDefinition,
  boundVariableNames: Seq[String],
  components: Seq[Template])
  extends StatementTemplate with DefinedExpressionTemplate
{
  override def withComponents(newComponents: Seq[Template]): DefinedStatementTemplate = copy(components = newComponents)
  override def names: Seq[String] = boundVariableNames ++ components.flatMap(_.names)
  override def matchExpression(
    expression: Expression,
    outerBoundVariableNames: Seq[Seq[String]],
    internalPath: Seq[Int]
  ): Option[Seq[Match]] = expression match {
    case definedStatement @ DefinedStatement(matchedComponents, `definition`) =>
      for {
        pairs <- components.zipStrict(matchedComponents)
        submatches <- pairs.mapWithIndex { case ((a, b), i) => a.matchExpression(b, outerBoundVariableNames :+ definedStatement.boundVariableNames, internalPath :+ i) }.traverseOption
      } yield definedStatement.boundVariableNames.mapWithIndex((name, index) => Template.Match.BoundVariable(name, index, internalPath)) ++ submatches.flatten
    case _ =>
      None
  }
  override def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): Template = {
    DefinedStatementTemplate(
      expressionDefinitionReplacements(definition).asInstanceOf[StatementDefinition],
      boundVariableNames,
      components.map(_.replaceDefinitions(expressionDefinitionReplacements)))
  }
  override def variables: Seq[VariableTemplate] = components.flatMap(_.variables)
  override def expand(statements: Map[String, Statement], terms: Map[String, Term]): Statement = {
    if (boundVariableNames.nonEmpty) throw new Exception("Parsing templated statements with bound variables not currently supported")
    definition(components.map(_.expand(statements, terms)):_*)
  }
  def expressionParserBuilder(statements: Map[String, Statement], terms: Map[String, Term])(implicit expressionParsingContext: ExpressionParsingContext): Parser[(DefinedStatement, Map[String, Statement], Map[String, Term])] = {
    for {
      parsedBoundVariableNames <- Parser.nWords(boundVariableNames.length)
      x <- components.zip(definition.componentTypes).foldLeft(Parser.constant((Seq.empty[Expression], statements, terms))) { case (currentParser, (componentTemplate, componentType)) =>
        for {
          x <- currentParser
          (currentComponents, currentStatements, currentTerms) = x
          y <- componentTemplate.expressionParserBuilder(currentStatements, currentTerms)(componentType.addParametersToContext(expressionParsingContext, parsedBoundVariableNames))
          (newComponent, newStatements, newTerms) = y
        } yield (currentComponents :+ newComponent, newStatements, newTerms)
      }
      (parsedComponents, resultStatements, resultTerms) = x
    } yield (DefinedStatement(parsedComponents, definition)(parsedBoundVariableNames), resultStatements, resultTerms)
  }
  override def referencedDefinitions: Set[ExpressionDefinition] = components.flatMap(_.referencedDefinitions).toSet + definition
  override def serialized: String = (Seq(definition.symbol) ++ boundVariableNames ++ components.map(_.serialized)).mkString(" ")
}

case class DefinedTermTemplate(
  definition: TermDefinition,
  boundVariableNames: Seq[String],
  components: Seq[Template])
  extends TermTemplate with DefinedExpressionTemplate
{
  override def withComponents(newComponents: Seq[Template]): DefinedTermTemplate = copy(components = newComponents)
  override def names: Seq[String] = boundVariableNames ++ components.flatMap(_.names)
  override def matchExpression(
    expression: Expression,
    outerBoundVariableNames: Seq[Seq[String]],
    internalPath: Seq[Int]
  ): Option[Seq[Match]] = expression match {
    case definedTerm @ DefinedTerm(matchedComponents, `definition`) =>
      for {
        pairs <- components.zipStrict(matchedComponents)
        submatches <- pairs.mapWithIndex { case ((a, b), i) => a.matchExpression(b, outerBoundVariableNames :+ definedTerm.boundVariableNames, internalPath :+ i) }.traverseOption
      } yield definedTerm.boundVariableNames.mapWithIndex((name, index) => Template.Match.BoundVariable(name, index, internalPath)) ++ submatches.flatten
    case _ =>
      None
  }
  override def replaceDefinitions(expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition]): Template = {
    DefinedTermTemplate(
      expressionDefinitionReplacements(definition).asInstanceOf[TermDefinition],
      boundVariableNames,
      components.map(_.replaceDefinitions(expressionDefinitionReplacements)))
  }
  override def referencedDefinitions: Set[ExpressionDefinition] = components.flatMap(_.referencedDefinitions).toSet + definition
  override def serialized: String = (Seq(definition.symbol) ++ boundVariableNames ++ components.map(_.serialized)).mkString(" ")
  override def variables: Seq[VariableTemplate] = components.flatMap(_.variables)
  override def expand(statements: Map[String, Statement], terms: Map[String, Term]): Term = {
    if (boundVariableNames.nonEmpty) throw new Exception("Parsing templated terms with bound variables not currently supported")
    definition(components.map(_.expand(statements, terms)):_*)
  }
  def expressionParserBuilder(statements: Map[String, Statement], terms: Map[String, Term])(implicit expressionParsingContext: ExpressionParsingContext): Parser[(DefinedTerm, Map[String, Statement], Map[String, Term])] = {
    for {
      parsedBoundVariableNames <- Parser.nWords(boundVariableNames.length)
      x <- components.zip(definition.componentTypes).foldLeft(Parser.constant((Seq.empty[Expression], statements, terms))) { case (currentParser, (componentTemplate, componentType)) =>
        for {
          x <- currentParser
          (currentComponents, currentStatements, currentTerms) = x
          y <- componentTemplate.expressionParserBuilder(currentStatements, currentTerms)(componentType.addParametersToContext(expressionParsingContext, parsedBoundVariableNames))
          (newComponent, newStatements, newTerms) = y
        } yield (currentComponents :+ newComponent, newStatements, newTerms)
      }
      (parsedComponents, resultStatements, resultTerms) = x
    } yield (DefinedTerm(parsedComponents, definition)(parsedBoundVariableNames), resultStatements, resultTerms)
  }
}

object Template {
  sealed trait Match
  object Match {
    case class BoundVariable(name: String, index: Int, internalPath: Seq[Int]) extends Match
    case class Component(expression: Expression, boundVariableNames: Seq[Seq[String]], internalPath: Seq[Int]) extends Match
  }

  def parser(implicit availableEntries: AvailableEntries): Parser[Template] = {
    implicit val templateParsingContext: TemplateParsingContext = TemplateParsingContext(availableEntries, Nil)
    Parser.selectWordParser("template")(Statement.templateParserFunction orElse Term.templateParserFunction)
  }
}

private class TemplateSerializer extends JsonSerializer[Template] {
  override def serialize(value: Template, gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeString(value.serialized)
  }
}
