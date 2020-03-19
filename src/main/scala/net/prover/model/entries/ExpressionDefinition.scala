package net.prover.model.entries

import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.expressions._
import net.prover.model._

trait ExpressionDefinition extends TypedExpressionDefinition[ExpressionDefinition] with ChapterEntry.Standalone

trait TypedExpressionDefinition[+ExpressionDefinitionType <: ExpressionDefinition] extends ChapterEntry.Standalone { self: ExpressionDefinition =>
  def symbol: String
  def boundVariableNames: Seq[String]
  def componentTypes: Seq[ComponentType]
  def format: Format
  def shorthand: Option[String]
  def defaultValue: Expression
  def typeName: String
  def attributes: Seq[String]
  def complexity: Int

  def withSymbol(newSymbol: String): ExpressionDefinitionType
  def withName(newName: Option[String]): ExpressionDefinitionType
  def withShorthand(newShorthand: Option[String]): ExpressionDefinitionType
  def withAttributes(newAttributes: Seq[String]): ExpressionDefinitionType
  def withFormat(newFormat: Format): ExpressionDefinitionType

  override def title: String = s"$typeName Definition: $name"
  override def referencedInferenceIds: Set[String] = Set.empty

  protected def componentExpressionParser(implicit context: ExpressionParsingContext): Parser[(Seq[String], Seq[Expression])] = {
    for {
      newBoundVariableNames <- Parser.nWords(boundVariableNames.length)
      components <- componentTypes.map { componentType =>
        componentType.expressionParser(componentType.addParametersToContext(context, newBoundVariableNames))
      }.traverseParser
    } yield (newBoundVariableNames, components)
  }

  protected def componentTemplateParser(implicit context: TemplateParsingContext): Parser[(Seq[String], Seq[Template])] = {
    for {
      newBoundVariableNames <- Parser.nWords(boundVariableNames.length)
      components <- componentTypes.map { componentType =>
        componentType.templateParser(componentType.addParametersToContext(context, newBoundVariableNames))
      }.traverseParser
    } yield (newBoundVariableNames, components)
  }

  def increaseDepth(internalDepth: Int) = if (boundVariableNames.nonEmpty) internalDepth + 1 else internalDepth

  protected def serializedComponents = "(" + (boundVariableNames.map("$" + _) ++ componentTypes.map(_.serialized)).mkString(" ") + ")"
}

object ExpressionDefinition {

  case class ComponentArgument(name: String, index: Int)

  sealed trait ComponentType {
    def name: String
    def withName(newName: String): ComponentType
    def expression: Expression
    def arguments: Seq[ComponentArgument]
    def expressionParser(implicit context: ExpressionParsingContext): Parser[Expression]
    def templateParser(implicit context: TemplateParsingContext): Parser[Template]
    def addParametersToContext[T <: ParsingContextWithParameters[T]](context: T, boundVariableNames: Seq[String]): T = {
      if (boundVariableNames.nonEmpty)
        context.addInnerParameters(getParameters(boundVariableNames))
      else
        context
    }
    def getParameters(boundVariableNames: Seq[String]): Seq[(String, Int)] = {
      arguments.map(a => boundVariableNames(a.index) -> a.index)
    }
    def serialized: String
  }
  object ComponentType {
    def listWithoutBoundVariablesParser: Parser[Seq[ComponentType]] = {
      Parser.selectOptionalWordParser {
        case ExpressionParsingContext.RecognisedStatementVariableName(name) =>
          Parser.constant(StatementComponent(name, Nil))
        case ExpressionParsingContext.RecognisedDefaultTermVariableNameWithSuffix(name) =>
          Parser.constant(TermComponent(name, Nil))
      }.whileDefined
    }

    def listParser(boundVariableNames: Seq[String]): Parser[Seq[ComponentType]] = {
      Parser.selectOptionalWordParser {
        case ExpressionParsingContext.RecognisedStatementVariableName(name) =>
          Parser.constant(StatementComponent(name, Nil))
        case ExpressionParsingContext.RecognisedDefaultTermVariableNameWithSuffix(name) =>
          Parser.constant(TermComponent(name, Nil))
        case "with" =>
          for {
            arguments <- Parser.selectWord("argument") {
              case name if boundVariableNames.contains(name) =>
                ComponentArgument(name, boundVariableNames.indexOf(name))
            }.listOrSingle(None)
            componentType <- Parser.selectWord("predicate or function name") {
              case ExpressionParsingContext.RecognisedStatementVariableName(name) =>
                StatementComponent(name, arguments)
              case ExpressionParsingContext.RecognisedDefaultTermVariableName(name) =>
                TermComponent(name, arguments)
            }
          } yield componentType
      }.whileDefined
    }

    case class StatementComponent(name: String, arguments: Seq[ComponentArgument]) extends ComponentType {
      override def withName(newName: String): StatementComponent = copy(name = newName)
      override def expression = StatementVariable(name, arguments.map(a => FunctionParameter(a.index, 0)))
      override def expressionParser(implicit context: ExpressionParsingContext) = Statement.parser
      override def templateParser(implicit context: TemplateParsingContext) = Statement.templateParser
      override def serialized = arguments match {
        case Nil => name
        case Seq(single) => s"with ${single.name} $name"
        case multiple => s"with (${multiple.map(_.name).mkString(" ")}) $name"
      }
    }
    case class TermComponent(name: String, arguments: Seq[ComponentArgument]) extends ComponentType {
      override def withName(newName: String): TermComponent = copy(name = newName)
      override def expression = TermVariable(name, arguments.map(a => FunctionParameter(a.index, 0)))
      override def expressionParser(implicit context: ExpressionParsingContext) = Term.parser
      override def templateParser(implicit context: TemplateParsingContext) = Term.templateParser
      override def serialized = arguments match {
        case Nil => name
        case Seq(single) => s"with ${single.name} $name"
        case multiple => s"with (${multiple.map(_.name).mkString(" ")}) $name"
      }
    }
  }

  private def boundVariablesParser: Parser[Seq[String]] = {
    val boundVariablePattern = "\\$(.*)".r
    Parser.selectOptionalWord {
      case boundVariablePattern(variableName) => variableName
    }.whileDefined
  }

  def rawBoundVariablesAndComponentTypesParser: Parser[(Seq[String], Seq[ComponentType])] = {
    for {
      boundVariables <- boundVariablesParser
      componentTypes <- ComponentType.listParser(boundVariables)
    } yield (boundVariables, componentTypes)
  }

  def boundVariablesAndComponentTypesParser: Parser[(Seq[String], Seq[ComponentType])] = {
    rawBoundVariablesAndComponentTypesParser.inParens
  }

  def shorthandParser: Parser[Option[String]] = Parser.optional("shorthand", Parser.allInParens)

  def attributesParser: Parser[Seq[String]] = {
    Parser.optional("attributes", Parser.allInParens.map(_.splitByWhitespace()), Nil)
  }
}
