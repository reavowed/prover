package net.prover.model.entries

import net.prover.model.{Format, Parser, ParsingContext}
import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.expressions._

trait ExpressionDefinition {
  def symbol: String
  def boundVariableNames: Seq[String]
  def componentTypes: Seq[ComponentType]
  def format: Format

  protected def serializedComponents = "(" + (boundVariableNames.map("$" + _) ++ componentTypes.map(_.serialized)).mkString(" ") + ")"
}

object ExpressionDefinition {

  case class ComponentArgument(name: String, index: Int)

  sealed trait ComponentType {
    def name: String
    def depthDifference: Int
    def expression: Expression
    def expressionParser(boundVariableNames: Seq[String])(implicit context: ParsingContext): Parser[Expression]
    def templateParser(boundVariableNames: Seq[String])(implicit context: ParsingContext): Parser[Template]
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
      }.collectWhileDefined
    }
  }

  case class StatementComponent(name: String) extends ComponentType {
    override def depthDifference = 0
    override def expression = StatementVariable(name, 0)
    override def expressionParser(boundVariableNames: Seq[String])(implicit context: ParsingContext) = Statement.parser
    override def templateParser(boundVariableNames: Seq[String])(implicit context: ParsingContext) = Statement.templateParser
    override def serialized = name
  }
  case class TermComponent(name: String) extends ComponentType {
    override def depthDifference = 0
    override def expression = TermVariable(name, 0)
    override def expressionParser(boundVariableNames: Seq[String])(implicit context: ParsingContext) = Term.parser
    override def templateParser(boundVariableNames: Seq[String])(implicit context: ParsingContext) = Term.templateParser
    override def serialized = name
  }
  case class PredicateComponent(name: String, arguments: Seq[ComponentArgument]) extends ComponentType {
    override def depthDifference = 1
    override def expression = PredicateApplication(name, ArgumentList(arguments.map(a => FunctionParameter(a.name, a.index)), 1))
    override def expressionParser(boundVariableNames: Seq[String])(implicit context: ParsingContext) = {
      Statement.parser(context.addParameterList(arguments.map { a => boundVariableNames(a.index) }))
    }
    override def templateParser(boundVariableNames: Seq[String])(implicit context: ParsingContext) = {
      Statement.templateParser(context.addParameterList(arguments.map { a => boundVariableNames(a.index) }))
    }
    override def serialized = "with " + (arguments match {
      case Seq(single) => single.name
      case multiple => "(" + multiple.map(_.name).mkString(" ") + ")"
    }) + " " + name
  }
  case class FunctionComponent(name: String, arguments: Seq[ComponentArgument]) extends ComponentType {
    override def depthDifference = 1
    override def expression = FunctionApplication(name, ArgumentList(arguments.map(a => FunctionParameter(a.name, a.index)), 1))
    override def expressionParser(boundVariableNames: Seq[String])(implicit context: ParsingContext) = {
      Term.parser(context.addParameterList(arguments.map { a => boundVariableNames(a.index) }))
    }
    override def templateParser(boundVariableNames: Seq[String])(implicit context: ParsingContext) = {
      Term.templateParser(context.addParameterList(arguments.map { a => boundVariableNames(a.index) }))
    }
    override def serialized = "with " + (arguments match {
      case Seq(single) => single.name
      case multiple => "(" + multiple.map(_.name).mkString(" ") + ")"
    }) + " " + name
  }

  private def boundVariablesParser(implicit context: ParsingContext): Parser[Seq[String]] = {
    val boundVariablePattern = "\\$(.*)".r
    Parser.selectOptionalWord {
      case boundVariablePattern(variableName) => variableName
    }.collectWhileDefined
  }

  def boundVariablesAndComponentTypesParser(implicit context: ParsingContext): Parser[(Seq[String], Seq[ComponentType])] = {
    (for {
      boundVariables <- boundVariablesParser
      componentTypes <- ComponentType.listParser(boundVariables)
    } yield (boundVariables, componentTypes)).inParens
  }
}