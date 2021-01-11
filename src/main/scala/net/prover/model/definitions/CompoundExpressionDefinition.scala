package net.prover.model.definitions

import com.fasterxml.jackson.annotation.JsonIgnore
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.core.expressions._
import net.prover.model._
import net.prover.model.definitions.CompoundExpressionDefinition.ComponentType
import net.prover.model.substitutions.Specifier
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
  def defaultComponentExpressions: Seq[ExpressionVariable[_, _]] = componentTypes.mapFold((0, 0)) {
    case ((statementCounter, termCounter), ComponentType.StatementComponent(_, arguments)) =>
      ((statementCounter + 1, termCounter), StatementVariable(statementCounter, arguments.map(a => Parameter(a.index, 0))))
    case ((statementCounter, termCounter), ComponentType.TermComponent(_, arguments)) =>
      ((statementCounter, termCounter + 1), TermVariable(termCounter, arguments.map(a => Parameter(a.index, 0))))
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
}

object CompoundExpressionDefinition {
  case class ComponentArgument(name: String, index: Int)
  sealed trait ComponentType {
    def name: String
    def arguments: Seq[ComponentArgument]
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
      override def serialized = arguments match {
        case Nil => name
        case Seq(single) => s"with ${single.name} $name"
        case multiple => s"with (${multiple.map(_.name).mkString(" ")}) $name"
      }
    }
    case class TermComponent(name: String, arguments: Seq[ComponentArgument]) extends ComponentType {
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
  val defaultValue: CompoundStatement = CompoundStatement(this, defaultComponentExpressions)(boundVariableNames)
  val constructionInference: Option[Inference.StatementDefinition] = definingStatement.map(Inference.StatementDefinition(name, variableDefinitions,  _, defaultValue))
  val deconstructionInference: Option[Inference.StatementDefinition] = definingStatement.map(Inference.StatementDefinition(name, variableDefinitions, defaultValue, _))
  def inferences: Seq[Inference.FromEntry] = constructionInference.toSeq ++ deconstructionInference.toSeq

  def apply(components: Expression*): CompoundStatement = {
    CompoundStatement(this, components)(boundVariableNames)
  }
  def bind(boundVariableNames: String*)(components: Expression*): CompoundStatement = {
    CompoundStatement(this, components)(boundVariableNames)
  }
  def unapplySeq(expression: Expression): Option[Seq[Expression]] = expression match {
    case CompoundStatement(definition, components) if definition == this =>
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

trait CompoundTermDefinition extends CompoundExpressionDefinition with CompoundTermType {
  def premises: Seq[Statement]
  def definitionPredicate: Statement
  val defaultValue: CompoundTerm = CompoundTerm(this, defaultComponentExpressions)(boundVariableNames)
  val definingStatement: Statement = Specifier.specifyOutsideProof(definitionPredicate, Seq(defaultValue)).get
  val definitionInference: Inference.Definition = Inference.TermDefinition(name, variableDefinitions, premises, definingStatement)
  def inferences: Seq[Inference.FromEntry] = Seq(definitionInference)

  def apply(components: Expression*): CompoundTerm = {
    CompoundTerm(this, components)(boundVariableNames)
  }
  def bind(boundVariableNames: String*)(components: Expression*): CompoundTerm = {
    CompoundTerm(this, components)(boundVariableNames)
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
