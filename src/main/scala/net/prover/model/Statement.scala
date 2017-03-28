package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

import scala.collection.immutable.Nil

trait Statement extends JsonSerializable.Base with Component {
  override val componentType = Statement
  override def serialize(gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeString(html)
  }
  override def serializeWithType(gen: JsonGenerator, serializers: SerializerProvider, typeSer: TypeSerializer): Unit = {
    serialize(gen, serializers)
  }
  def containsTerms: Boolean
  def applySubstitutions(substitutions: Substitutions): Statement
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable): Statement
  def resolveSingleSubstitution(other: Component, termVariable: TermVariable, thisTerm: Term, otherTerm: Term): Option[Statement]
}

case class StatementVariable(text: String) extends Statement {
  override def variables: Variables = Variables(Seq(this), Nil)
  override def allBoundVariables: Seq[TermVariable] = Nil
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Option[PartialSubstitutions] = other match {
    case otherStatement: Statement =>
      substitutions.tryAdd(this, otherStatement)
    case _ =>
      None
  }
  override def applySubstitutions(substitutions: Substitutions): Statement = {
    substitutions.statements.getOrElse(this, throw new Exception(s"No replacement for statement variable $this"))
  }

  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable) = {
    StatementVariableWithSingleSubstitution(this, termToReplaceWith, termToBeReplaced)
  }
  def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[Statement] = {
    if (this == other) {
      Some(this)
    } else {
      None
    }
  }
  def findSubstitution(other: Component, termVariable: TermVariable): Option[Option[Term]] = {
    if (this == other) {
      Some(None)
    } else {
      None
    }
  }

  override def containsTerms = false

  override def html: String = text

  override def serialized: String = text
}

case class StatementVariableWithSingleSubstitution(
    statementVariable: StatementVariable,
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable)
 extends Statement
{
  override def variables: Variables = termToReplaceWith.variables :+ statementVariable
  override def allBoundVariables: Seq[TermVariable] = termToReplaceWith.allBoundVariables
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Option[PartialSubstitutions] = other match {
    case StatementVariableWithSingleSubstitution(otherStatementVariable, otherTermToReplaceWith, otherTermToBeReplaced) =>
      for {
        s1 <- statementVariable.calculateSubstitutions(otherStatementVariable, substitutions)
        s2 <- termToReplaceWith.calculateSubstitutions(otherTermToReplaceWith, s1)
        s3 <- termToBeReplaced.calculateSubstitutions(otherTermToBeReplaced, s2)
      } yield s3
    case statement: Statement =>
      substitutions.tryAdd(this, statement)
    case _ =>
      None
  }
  override def applySubstitutions(substitutions: Substitutions): Statement = {
    statementVariable.applySubstitutions(substitutions).makeSingleSubstitution(termToReplaceWith, termToBeReplaced)
  }
  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable): Statement = {
    throw new Exception("Cannot make multiple substitutions")
  }
  def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[Statement] = {
    None // TODO: overly conservative
  }
  def findSubstitution(other: Component, termVariable: TermVariable): Option[Option[Term]] = {
    if (this == other) {
      Some(None)
    } else {
      None
    }
  }

  override def containsTerms = false

  override def html: String = "[" + termToReplaceWith.safeHtml + "/" + termToBeReplaced.html + "]" + statementVariable.html

  override def serialized: String = Seq(
    "sub",
    termToReplaceWith.serialized,
    termToBeReplaced.serialized,
    statementVariable.serialized
  ).mkString(" ")
}

case class DefinedStatement(
    subcomponents: Seq[Component],
    boundVariables: Seq[TermVariable],
    definition: StatementDefinition)
 extends Statement
{
  override def variables: Variables = subcomponents.map(_.variables).foldLeft(Variables.empty)(_ ++ _)

  override def allBoundVariables = boundVariables ++ subcomponents.flatMap(_.allBoundVariables)

  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Option[PartialSubstitutions] = other match {
    case DefinedStatement(otherSubcomponents, _, `definition`) =>
      subcomponents.zip(otherSubcomponents)
        .foldLeft(Option(substitutions)) { case (substitutionsSoFarOption, (component, otherComponent)) =>
          substitutionsSoFarOption.flatMap(component.calculateSubstitutions(otherComponent, _))
        }
    case _ =>
      None
  }

  override def applySubstitutions(substitutions: Substitutions): Statement = {
    val newBoundVariables = boundVariables.map(_.applySubstitutions(substitutions)).map(Term.asVariable)
//    for (variable <- variables.statementVariables) {
//      if (substitutions.statements(variable).variables.termVariables.intersect(newBoundVariables).nonEmpty)
//        throw new Exception("Cannot substitute a new instance of a bound variable")
//    }
//    for (variable <- variables.termVariables) {
//      if (substitutions.terms(variable).variables.termVariables.intersect(newBoundVariables).nonEmpty)
//        throw new Exception("Cannot substitute a new instance of a bound variable")
//    }
    copy(
      subcomponents = subcomponents.map(_.applySubstitutions(substitutions)),
      boundVariables = boundVariables.map(_.applySubstitutions(substitutions)).map(Term.asVariable))
  }

  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable): Statement = {
    if (boundVariables.contains(termToBeReplaced))
      throw new Exception("Cannot substitute for a bound variable")
    if (termToReplaceWith.variables.termVariables.intersect(boundVariables).nonEmpty)
      throw new Exception("Cannot substitute a new instance of a bound variable")
    copy(subcomponents = subcomponents.map(_.makeSingleSubstitution(termToReplaceWith, termToBeReplaced)))
  }
  def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[Statement] = {
    other match {
      case DefinedStatement(otherSubcomponents, otherBoundVariables, `definition`) =>
        if (boundVariables.contains(termVariable) || otherBoundVariables.contains(termVariable))
          None
        else {
          subcomponents.zip(otherSubcomponents)
            .map { case (subcomponent, otherSubcomponent) =>
              subcomponent.resolveSingleSubstitution(otherSubcomponent, termVariable, thisTerm, otherTerm)
            }
            .traverseOption
            .map { resolvedSubcomponents =>
              copy(subcomponents = resolvedSubcomponents)
            }
        }
      case _ =>
        None
    }
  }
  def findSubstitution(other: Component, termVariable: TermVariable): Option[Option[Term]] = {
    other match {
      case DefinedStatement(otherSubcomponents, _, `definition`) =>
        val x = subcomponents.zip(otherSubcomponents)
          .map {
            case (subcomponent, otherSubcomponent) =>
              subcomponent.findSubstitution(otherSubcomponent, termVariable)
          }
          .traverseOption
          .flatMap { y =>
            y.flatten.distinct match {
              case Seq(singleTerm) =>
                Some(Some(singleTerm))
              case Nil =>
                Some(None)
              case _ =>
                None
            }
          }
        x
      case _ =>
        None
    }
  }

  override def html: String = {
    definition.format.html(subcomponents)
  }
  override def safeHtml: String = {
    definition.format.safeHtml(subcomponents)
  }
  override def serialized: String = (definition.symbol +: subcomponents.map(_.serialized)).mkString(" ")
  override def containsTerms: Boolean = subcomponents.exists {
    case s: Statement =>
      s.containsTerms
    case _: Term =>
      true
  }
}

object Statement extends ComponentType {

  def parser(implicit context: Context): Parser[Statement] = {
    object ParsableStatement {
      def unapply(s: String): Option[StatementDefinition] = {
        context.statementDefinitions.find(_.symbol == s)
      }
    }
    object SpecifiedVariable {
      def unapply(s: String): Option[StatementVariable] = {
        context.variables.statementVariables.find(_.text == s)
      }
    }

    def parserForStatementType(statementType: String): Parser[Statement] = statementType match {
      case ParsableStatement(statementDefinition) =>
        statementDefinition.statementParser
      case SpecifiedVariable(v) =>
        Parser.constant(v)
      case "sub" =>
        for {
          termToReplaceWith <- Term.parser
          termToBeReplaced <- Term.variableParser
          statement <- parser
        } yield {
          statement.makeSingleSubstitution(termToReplaceWith, termToBeReplaced)
        }
      case _ =>
        throw new Exception(s"Unrecognised statement type $statementType")
    }

    Parser.singleWord.flatMap(parserForStatementType)
  }

  def listParser(implicit context: Context): Parser[Seq[Statement]] = parser.listInParens(Some(","))

  def variableParser(implicit context: Context): Parser[StatementVariable] = parser.map {
    case variable: StatementVariable =>
      variable
    case nonVariable =>
      throw new Exception(s"Expected statement variable, got $nonVariable")
  }
}
