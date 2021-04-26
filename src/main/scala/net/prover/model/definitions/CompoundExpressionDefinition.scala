package net.prover.model.definitions

import com.fasterxml.jackson.annotation.JsonIgnore
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.core.expressions.CompoundStatementType
import net.prover.core.transformers.ContextWithExternalDepth
import net.prover.model._
import net.prover.model.definitions.CompoundExpressionDefinition.ComponentType
import net.prover.model.expressions._
import net.prover.old.ExpressionSpecifier
import net.prover.structure.model.entries.ChapterEntry

trait CompoundExpressionDefinition {
  def baseSymbol: String
  def disambiguator: Option[String]
  def disambiguatedSymbol: DisambiguatedSymbol = DisambiguatedSymbol(baseSymbol, disambiguator)
  @JsonSerialize
  def symbol: String = disambiguatedSymbol.serialized
  def explicitName: Option[String]
  def name: String = explicitName.getOrElse(disambiguatedSymbol.forDisplay)
  def boundVariableNames: Seq[String]
  def hasBoundVariables: Boolean = boundVariableNames.nonEmpty
  def componentTypes: Seq[ComponentType]
  def defaultComponentExpressions: Seq[ExpressionVariable[_]] = componentTypes.mapFold((0, 0)) {
    case ((statementCounter, termCounter), ComponentType.StatementComponent(_, arguments)) =>
      ((statementCounter + 1, termCounter), StatementVariable(statementCounter, arguments.map(a => FunctionParameter(a.index, 0))))
    case ((statementCounter, termCounter), ComponentType.TermComponent(_, arguments)) =>
      ((statementCounter, termCounter + 1), TermVariable(termCounter, arguments.map(a => FunctionParameter(a.index, 0))))
  }._2
  def format: Format
  def shorthand: Option[String]
  def defaultValue: Expression
  def attributes: Seq[String]
  @JsonIgnore
  def associatedChapterEntry: ChapterEntry

  def variableDefinitions: VariableDefinitions = VariableDefinitions.fromComponentTypes(componentTypes)

  def increaseDepth(internalDepth: Int): Int = {
    if (boundVariableNames.nonEmpty)
      internalDepth + 1
    else
      internalDepth
  }

  protected def componentExpressionParser(implicit context: ExpressionParsingContext): Parser[(Seq[String], Seq[Expression])] = {
    for {
      newBoundVariableNames <- Parser.nWords(boundVariableNames.length)
      components <- componentTypes.map { componentType =>
        componentType.expressionParser(componentType.addParametersToContext(context, newBoundVariableNames))
      }.traverse
    } yield (newBoundVariableNames, components)
  }

  protected def componentTemplateParser(implicit context: TemplateParsingContext): Parser[(Seq[String], Seq[Template])] = {
    for {
      newBoundVariableNames <- Parser.nWords(boundVariableNames.length)
      components <- componentTypes.map { componentType =>
        componentType.templateParser(componentType.addParametersToContext(context, newBoundVariableNames))
      }.traverse
    } yield (newBoundVariableNames, components)
  }
}

object CompoundExpressionDefinition {
  case class ComponentArgument(name: String, index: Int)
  sealed trait ComponentType {
    def name: String
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
        case ExpressionParsingContext.RecognisedTermVariableName(name) =>
          Parser.constant(TermComponent(name, Nil))
      }.whileDefined
    }

    def listParser(boundVariableNames: Seq[String]): Parser[Seq[ComponentType]] = {
      Parser.selectOptionalWordParser {
        case ExpressionParsingContext.RecognisedStatementVariableName(name) =>
          Parser.constant(StatementComponent(name, Nil))
        case ExpressionParsingContext.RecognisedTermVariableName(name) =>
          Parser.constant(TermComponent(name, Nil))
        case "with" =>
          for {
            arguments <- Parser.selectWord("argument") {
              case name if boundVariableNames.contains(name) =>
                ComponentArgument(name, boundVariableNames.indexOf(name))
            }.listInParensOrSingle(None)
            componentType <- Parser.selectWord("predicate or function name") {
              case ExpressionParsingContext.RecognisedStatementVariableName(name) =>
                StatementComponent(name, arguments)
              case ExpressionParsingContext.RecognisedTermVariableName(name) =>
                TermComponent(name, arguments)
            }
          } yield componentType
      }.whileDefined
    }

    case class StatementComponent(name: String, arguments: Seq[ComponentArgument]) extends ComponentType {
      override def expressionParser(implicit context: ExpressionParsingContext) = Statement.parser
      override def templateParser(implicit context: TemplateParsingContext) = Statement.templateParser
      override def serialized = arguments match {
        case Nil => name
        case Seq(single) => s"with ${single.name} $name"
        case multiple => s"with (${multiple.map(_.name).mkString(" ")}) $name"
      }
    }
    case class TermComponent(name: String, arguments: Seq[ComponentArgument]) extends ComponentType {
      override def expressionParser(implicit context: ExpressionParsingContext) = Term.parser
      override def templateParser(implicit context: TemplateParsingContext) = Term.templateParser
      override def serialized = arguments match {
        case Nil => name
        case Seq(single) => s"with ${single.name} $name"
        case multiple => s"with (${multiple.map(_.name).mkString(" ")}) $name"
      }
    }
  }
}

trait CompoundStatementDefinition extends CompoundExpressionDefinition with CompoundStatementType {
  def definingStatement: Option[Statement]
  val disambiguator: Option[String] = None
  val defaultValue: DefinedStatement = DefinedStatement(defaultComponentExpressions, this)(boundVariableNames)
  val constructionInference: Option[Inference.StatementDefinition] = definingStatement.map(Inference.StatementDefinition(name, variableDefinitions,  _, defaultValue))
  val deconstructionInference: Option[Inference.StatementDefinition] = definingStatement.map(Inference.StatementDefinition(name, variableDefinitions, defaultValue, _))
  def inferences: Seq[Inference.FromEntry] = constructionInference.toSeq ++ deconstructionInference.toSeq

  def statementParser(implicit context: ExpressionParsingContext): Parser[Statement] = {
    componentExpressionParser.map { case (newBoundVariableNames, components) =>
      DefinedStatement(components, this)(newBoundVariableNames)
    }
  }
  def templateParser(implicit templateParsingContext: TemplateParsingContext): Parser[Template] = {
    componentTemplateParser.map { case (newBoundVariableNames, components) =>
      DefinedStatementTemplate(this, newBoundVariableNames, components)
    }
  }

  def apply(components: Expression*): DefinedStatement = {
    DefinedStatement(components, this)(boundVariableNames)
  }
  def bind(boundVariableNames: String*)(components: Expression*): DefinedStatement = {
    DefinedStatement(components, this)(boundVariableNames)
  }
  def unapplySeq(expression: Expression): Option[Seq[Expression]] = expression match {
    case DefinedStatement(components, definition) if definition == this =>
      Some(components)
    case _ =>
      None
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[CompoundStatementDefinition]

  override def equals(other: Any): Boolean = other match {
    case that: CompoundStatementDefinition =>
      (that canEqual this) &&
        symbol == that.symbol
    case _ => false
  }

  override val hashCode: Int = symbol.hashCode
}
object CompoundStatementDefinition {
  case class Derived(baseSymbol: String, componentTypes: Seq[ComponentType], explicitName: Option[String], format: Format, definingStatement: Option[Statement], associatedChapterEntry: ChapterEntry) extends CompoundStatementDefinition {
    override def boundVariableNames: Seq[String] = Nil
    override def shorthand: Option[String] = None
    override def attributes: Seq[String] = Nil
  }
}

trait CompoundTermDefinition extends CompoundExpressionDefinition {
  def premises: Seq[Statement]
  def definitionPredicate: Statement
  val defaultValue: DefinedTerm = DefinedTerm(defaultComponentExpressions, this)(boundVariableNames)
  val definingStatement: Statement = ExpressionSpecifier.specify(definitionPredicate, Seq(defaultValue))(ContextWithExternalDepth.zero).get
  val definitionInference: Inference.Definition = Inference.TermDefinition(name, variableDefinitions, premises, definingStatement)
  def inferences: Seq[Inference.FromEntry] = Seq(definitionInference)

  def termParser(implicit context: ExpressionParsingContext): Parser[Term] = {
    componentExpressionParser.map { case (newBoundVariableNames, components) =>
      DefinedTerm(components, this)(newBoundVariableNames)
    }
  }

  def templateParser(implicit context: TemplateParsingContext): Parser[Template] = {
    componentTemplateParser.map { case (newBoundVariableNames, components) =>
      DefinedTermTemplate(this, newBoundVariableNames, components)
    }
  }

  def apply(components: Expression*): DefinedTerm = {
    DefinedTerm(components, this)(boundVariableNames)
  }
  def bind(boundVariableNames: String*)(components: Expression*): DefinedTerm = {
    DefinedTerm(components, this)(boundVariableNames)
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[CompoundTermDefinition]

  override def equals(other: Any): Boolean = other match {
    case that: CompoundTermDefinition =>
      (that canEqual this) &&
        symbol == that.symbol
    case _ => false
  }

  override val hashCode: Int = symbol.hashCode
}
