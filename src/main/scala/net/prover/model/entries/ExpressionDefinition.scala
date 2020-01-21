package net.prover.model.entries

import com.fasterxml.jackson.databind.annotation.JsonSerialize
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
  def withShorthand(newShorthand: Option[String]): ExpressionDefinitionType
  def withAttributes(newAttributes: Seq[String]): ExpressionDefinitionType

  @JsonSerialize
  override def title: String = s"$typeName Definition: $name"
  override def referencedInferenceIds: Set[String] = Set.empty

  private def addBoundVariableList(
    existingBoundVariableLists: Seq[Seq[(String, Int)]],
    newBoundVariableNames: Seq[String],
    componentType: ComponentType
  ): Seq[Seq[(String, Int)]] = {
    if (boundVariableNames.isEmpty)
      existingBoundVariableLists
    else
      existingBoundVariableLists :+ componentType.arguments.map(a => newBoundVariableNames(a.index) -> a.index)
  }

  protected def componentExpressionParser(implicit context: ExpressionParsingContext): Parser[(Seq[String], Seq[Expression])] = {
    for {
      newBoundVariableNames <- Parser.nWords(boundVariableNames.length)
      components <- componentTypes.map { componentType =>
        componentType.expressionParser(context.copy(parameterLists = addBoundVariableList(context.parameterLists, newBoundVariableNames, componentType)))
      }.traverseParser
    } yield (newBoundVariableNames, components)
  }

  protected def componentTemplateParser(implicit context: TemplateParsingContext): Parser[(Seq[String], Seq[Template])] = {
    for {
      newBoundVariableNames <- Parser.nWords(boundVariableNames.length)
      components <- componentTypes.map { componentType =>
        componentType.templateParser(context.copy(parameterLists = addBoundVariableList(context.parameterLists, newBoundVariableNames, componentType)))
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
    def serialized: String
  }
  object ComponentType {

    case class StatementComponent(name: String) extends ComponentType {
      override def withName(newName: String): StatementComponent = copy(name = newName)
      override def arguments = Nil
      override def expression = StatementVariable(name)
      override def expressionParser(implicit context: ExpressionParsingContext) = Statement.parser
      override def templateParser(implicit context: TemplateParsingContext) = Statement.templateParser
      override def serialized = name
    }
    case class TermComponent(name: String) extends ComponentType {
      override def withName(newName: String): TermComponent = copy(name = newName)
      override def arguments = Nil
      override def expression = TermVariable(name)
      override def expressionParser(implicit context: ExpressionParsingContext) = Term.parser
      override def templateParser(implicit context: TemplateParsingContext) = Term.templateParser
      override def serialized = name
    }
    case class PredicateComponent(name: String, arguments: Seq[ComponentArgument]) extends ComponentType {
      override def withName(newName: String): PredicateComponent = copy(name = newName)
      override def expression = PredicateApplication(name, arguments.map(a => FunctionParameter(a.index, 0)))
      override def expressionParser(implicit context: ExpressionParsingContext) = Statement.parser
      override def templateParser(implicit context: TemplateParsingContext) = Statement.templateParser
      override def serialized = "with " + (arguments match {
        case Seq(single) => single.name
        case multiple => "(" + multiple.map(_.name).mkString(" ") + ")"
      }) + " " + name
    }
    case class FunctionComponent(name: String, arguments: Seq[ComponentArgument]) extends ComponentType {
      override def withName(newName: String): FunctionComponent = copy(name = newName)
      override def expression = FunctionApplication(name, arguments.map(a => FunctionParameter(a.index, 0)))
      override def expressionParser(implicit context: ExpressionParsingContext) = Term.parser
      override def templateParser(implicit context: TemplateParsingContext) = Term.templateParser
      override def serialized = "with " + (arguments match {
        case Seq(single) => single.name
        case multiple => "(" + multiple.map(_.name).mkString(" ") + ")"
      }) + " " + name
    }

    def listWithoutBoundVariablesParser: Parser[Seq[ComponentType]] = {
      Parser.selectOptionalWordParser {
        case ExpressionParsingContext.RecognisedStatementVariableName(name) =>
          Parser.constant(StatementComponent(name))
        case ExpressionParsingContext.RecognisedDefaultTermVariableName(name) =>
          Parser.constant(TermComponent(name))
      }.whileDefined
    }
    def listParser(boundVariableNames: Seq[String]): Parser[Seq[ComponentType]] = {
      Parser.selectOptionalWordParser {
        case ExpressionParsingContext.RecognisedStatementVariableName(name) =>
          Parser.constant(StatementComponent(name))
        case ExpressionParsingContext.RecognisedDefaultTermVariableName(name) =>
          Parser.constant(TermComponent(name))
        case "with" =>
          for {
            arguments <- Parser.selectWord("argument") {
              case name if boundVariableNames.contains(name) =>
                ComponentArgument(name, boundVariableNames.indexOf(name))
            }.listOrSingle(None)
            componentType <- Parser.selectWord("predicate or function name") {
              case ExpressionParsingContext.RecognisedStatementVariableName(name) =>
                PredicateComponent(name, arguments)
              case ExpressionParsingContext.RecognisedDefaultTermVariableName(name) =>
                FunctionComponent(name, arguments)
            }
          } yield componentType
      }.whileDefined
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
