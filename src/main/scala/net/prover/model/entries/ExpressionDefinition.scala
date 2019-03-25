package net.prover.model.entries

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import net.prover.model.{Format, Parser, ParsingContext}
import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.expressions._

@JsonSerialize(using = classOf[ExpressionDefinitionSerializer])
trait ExpressionDefinition extends TypedExpressionDefinition[ExpressionDefinition]

trait TypedExpressionDefinition[+ExpressionDefinitionType <: ExpressionDefinition] extends ChapterEntry { self: ExpressionDefinition =>
  def symbol: String
  def key: ChapterEntry.Key.Anchor
  def boundVariableNames: Seq[String]
  def componentTypes: Seq[ComponentType]
  def format: Format
  def shorthand: Option[String]
  def defaultValue: Expression

  def withShorthand(newShorthand: Option[String]): ExpressionDefinitionType

  private def updateContext(context: ParsingContext, newBoundVariableNames: Seq[String], componentType: ComponentType): ParsingContext = {
    if (boundVariableNames.isEmpty)
      context
    else
      context.addParameterList(componentType.arguments.map(a => newBoundVariableNames(a.index) -> a.index))
  }

  protected def componentExpressionParser(implicit context: ParsingContext): Parser[(Seq[String], Seq[Expression])] = {
    for {
      newBoundVariableNames <- Parser.nWords(boundVariableNames.length)
      components <- componentTypes.map { componentType =>
        componentType.expressionParser(updateContext(context, newBoundVariableNames, componentType))
      }.traverseParser
    } yield (newBoundVariableNames, components)
  }

  protected def componentTemplateParser(implicit context: ParsingContext): Parser[(Seq[String], Seq[Template])] = {
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
    def expressionParser(implicit context: ParsingContext): Parser[Expression]
    def templateParser(implicit context: ParsingContext): Parser[Template]
    def serialized: String
  }
  object ComponentType {
    def listParser(boundVariableNames: Seq[String])(implicit context: ParsingContext): Parser[Seq[ComponentType]] = {
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
    override def expressionParser(implicit context: ParsingContext) = Statement.parser
    override def templateParser(implicit context: ParsingContext) = Statement.templateParser
    override def serialized = name
  }
  case class TermComponent(name: String) extends ComponentType {
    override def arguments = Nil
    override def expression = TermVariable(name)
    override def expressionParser(implicit context: ParsingContext) = Term.parser
    override def templateParser(implicit context: ParsingContext) = Term.templateParser
    override def serialized = name
  }
  case class PredicateComponent(name: String, arguments: Seq[ComponentArgument]) extends ComponentType {
    override def expression = PredicateApplication(name, arguments.map(a => FunctionParameter(a.index, 0)))
    override def expressionParser(implicit context: ParsingContext) = Statement.parser
    override def templateParser(implicit context: ParsingContext) = Statement.templateParser
    override def serialized = "with " + (arguments match {
      case Seq(single) => single.name
      case multiple => "(" + multiple.map(_.name).mkString(" ") + ")"
    }) + " " + name
  }
  case class FunctionComponent(name: String, arguments: Seq[ComponentArgument]) extends ComponentType {
    override def expression = FunctionApplication(name, arguments.map(a => FunctionParameter(a.index, 0)))
    override def expressionParser(implicit context: ParsingContext) = Term.parser
    override def templateParser(implicit context: ParsingContext) = Term.templateParser
    override def serialized = "with " + (arguments match {
      case Seq(single) => single.name
      case multiple => "(" + multiple.map(_.name).mkString(" ") + ")"
    }) + " " + name
  }

  private def boundVariablesParser(implicit context: ParsingContext): Parser[Seq[String]] = {
    val boundVariablePattern = "\\$(.*)".r
    Parser.selectOptionalWord {
      case boundVariablePattern(variableName) => variableName
    }.whileDefined
  }

  def boundVariablesAndComponentTypesParser(implicit context: ParsingContext): Parser[(Seq[String], Seq[ComponentType])] = {
    (for {
      boundVariables <- boundVariablesParser
      componentTypes <- ComponentType.listParser(boundVariables)
    } yield (boundVariables, componentTypes)).inParens
  }

  def shorthandParser = Parser.optional("shorthand", Parser.allInParens)
}

private class ExpressionDefinitionSerializer extends JsonSerializer[ExpressionDefinition] {
  override def serialize(value: ExpressionDefinition, gen: JsonGenerator, serializers: SerializerProvider) = {
    gen.writeStartObject(value)
    gen.writeObjectField("symbol", value.symbol)
    gen.writeObjectField("baseFormatString", value.format.baseFormatString)
    gen.writeObjectField("requiresBrackets", value.format.requiresBrackets)
    gen.writeObjectField("numberOfBoundVariables", value.boundVariableNames.length)
    value.asOptionalInstanceOf[StatementDefinition].flatMap(_.structureType).foreach(structureType => gen.writeObjectField("structureType", structureType.serialized))
    gen.writeEndObject()
  }
}
