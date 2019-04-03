package net.prover.model.entries

import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.expressions._
import net.prover.model.{ExpressionParsingContext, Format, Parser}

trait ExpressionDefinition extends TypedExpressionDefinition[ExpressionDefinition]

trait TypedExpressionDefinition[+ExpressionDefinitionType <: ExpressionDefinition] extends ChapterEntry.Standalone { self: ExpressionDefinition =>
  def symbol: String
  def boundVariableNames: Seq[String]
  def componentTypes: Seq[ComponentType]
  def format: Format
  def shorthand: Option[String]
  def defaultValue: Expression
  def typeName: String

  def withShorthand(newShorthand: Option[String]): ExpressionDefinitionType

  @JsonSerialize
  override def title: String = s"$typeName Definition: $name"
  override def referencedInferenceIds: Set[String] = Set.empty

  private def updateContext(
    context: ExpressionParsingContext,
    newBoundVariableNames: Seq[String],
    componentType: ComponentType
  ): ExpressionParsingContext = {
    if (boundVariableNames.isEmpty)
      context
    else
      context.addParameterList(componentType.arguments.map(a => newBoundVariableNames(a.index) -> a.index))
  }

  protected def componentExpressionParser(implicit context: ExpressionParsingContext): Parser[(Seq[String], Seq[Expression])] = {
    for {
      newBoundVariableNames <- Parser.nWords(boundVariableNames.length)
      components <- componentTypes.map { componentType =>
        componentType.expressionParser(updateContext(context, newBoundVariableNames, componentType))
      }.traverseParser
    } yield (newBoundVariableNames, components)
  }

  protected def componentTemplateParser(implicit context: ExpressionParsingContext): Parser[(Seq[String], Seq[Template])] = {
    for {
      newBoundVariableNames <- Parser.nWords(boundVariableNames.length)
      components <- componentTypes.map { componentType =>
        componentType.templateParser(updateContext(context, newBoundVariableNames, componentType))
      }.traverseParser
    } yield (newBoundVariableNames, components)
  }

  protected def serializedComponents = "(" + (boundVariableNames.map("$" + _) ++ componentTypes.map(_.serialized)).mkString(" ") + ")"
}

object ExpressionDefinition {

  case class ComponentArgument(name: String, index: Int)

  sealed trait ComponentType {
    def name: String
    def expression: Expression
    def arguments: Seq[ComponentArgument]
    def expressionParser(implicit context: ExpressionParsingContext): Parser[Expression]
    def templateParser(implicit context: ExpressionParsingContext): Parser[Template]
    def serialized: String
  }
  object ComponentType {
    def listParser(boundVariableNames: Seq[String])(implicit context: ExpressionParsingContext): Parser[Seq[ComponentType]] = {
      Parser.selectOptionalWordParser {
        case context.RecognisedStatementVariable(name) =>
          Parser.constant(StatementComponent(name))
        case context.RecognisedTermVariable(name) =>
          Parser.constant(TermComponent(name))
        case "with" =>
          for {
            arguments <- Parser.selectWord("argument") {
              case name if boundVariableNames.contains(name) =>
                ComponentArgument(name, boundVariableNames.indexOf(name))
            }.listOrSingle(None)
            componentType <- Parser.selectWord("predicate or function name") {
              case context.RecognisedStatementVariable(name) =>
                PredicateComponent(name, arguments)
            }
          } yield componentType
      }.whileDefined
    }
  }

  case class StatementComponent(name: String) extends ComponentType {
    override def arguments = Nil
    override def expression = StatementVariable(name)
    override def expressionParser(implicit context: ExpressionParsingContext) = Statement.parser
    override def templateParser(implicit context: ExpressionParsingContext) = Statement.templateParser
    override def serialized = name
  }
  case class TermComponent(name: String) extends ComponentType {
    override def arguments = Nil
    override def expression = TermVariable(name)
    override def expressionParser(implicit context: ExpressionParsingContext) = Term.parser
    override def templateParser(implicit context: ExpressionParsingContext) = Term.templateParser
    override def serialized = name
  }
  case class PredicateComponent(name: String, arguments: Seq[ComponentArgument]) extends ComponentType {
    override def expression = PredicateApplication(name, arguments.map(a => FunctionParameter(a.index, 0)))
    override def expressionParser(implicit context: ExpressionParsingContext) = Statement.parser
    override def templateParser(implicit context: ExpressionParsingContext) = Statement.templateParser
    override def serialized = "with " + (arguments match {
      case Seq(single) => single.name
      case multiple => "(" + multiple.map(_.name).mkString(" ") + ")"
    }) + " " + name
  }
  case class FunctionComponent(name: String, arguments: Seq[ComponentArgument]) extends ComponentType {
    override def expression = FunctionApplication(name, arguments.map(a => FunctionParameter(a.index, 0)))
    override def expressionParser(implicit context: ExpressionParsingContext) = Term.parser
    override def templateParser(implicit context: ExpressionParsingContext) = Term.templateParser
    override def serialized = "with " + (arguments match {
      case Seq(single) => single.name
      case multiple => "(" + multiple.map(_.name).mkString(" ") + ")"
    }) + " " + name
  }

  private def boundVariablesParser: Parser[Seq[String]] = {
    val boundVariablePattern = "\\$(.*)".r
    Parser.selectOptionalWord {
      case boundVariablePattern(variableName) => variableName
    }.whileDefined
  }

  def boundVariablesAndComponentTypesParser(implicit context: ExpressionParsingContext): Parser[(Seq[String], Seq[ComponentType])] = {
    (for {
      boundVariables <- boundVariablesParser
      componentTypes <- ComponentType.listParser(boundVariables)
    } yield (boundVariables, componentTypes)).inParens
  }

  def shorthandParser = Parser.optional("shorthand", Parser.allInParens)
}
