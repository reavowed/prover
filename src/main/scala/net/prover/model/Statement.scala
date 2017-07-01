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
  def applySubstitutions(substitutions: Substitutions): Option[Statement]
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[Statement]
  def resolveSingleSubstitution(other: Component, termVariable: TermVariable, thisTerm: Term, otherTerm: Term): Option[(Statement, DistinctVariables)]
  def replacePlaceholder(other: Component): Option[Statement]
}

case class StatementVariable(text: String) extends Statement with Variable {
  override def allVariables: Set[Variable] = Set(this)
  override def presentVariables: Set[Variable] = Set(this)
  override def boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = (Set.empty, Set.empty)
  override def implicitDistinctVariables: DistinctVariables = DistinctVariables.empty
  def getPotentiallyIntersectingVariables(termVariable: TermVariable): Set[Variable] = Set(this)
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Seq[PartialSubstitutions] = other match {
    case otherStatement: Statement =>
      substitutions.tryAdd(this, otherStatement).toSeq
    case _ =>
      Nil
  }
  def applySubstitutions(substitutions: Substitutions): Option[Statement] = {
    substitutions.componentsByVariable.get(this).map(_.asInstanceOf[Statement])
  }
  override def validateSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    target: Component,
    distinctVariables: DistinctVariables
  ): Option[DistinctVariables] = {
    if (target == this) {
      if (termToBeReplaced == termToReplaceWith)
        Some(DistinctVariables.empty)
      else
        Some(DistinctVariables(termToBeReplaced -> this))
    } else if (makeSingleSubstitution(termToReplaceWith, termToBeReplaced, distinctVariables).contains(target)) {
      Some(DistinctVariables.empty)
    } else {
      None
    }
  }
  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables) = {
    if (termToReplaceWith == termToBeReplaced)
      Some(this)
    else if (distinctVariables.areDistinct(termToBeReplaced, this))
      Some(this)
    else
      Some(SubstitutedStatementVariable(this, termToReplaceWith, termToBeReplaced))
  }
  override def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[(Statement, DistinctVariables)] = {
    if (this == other) {
      Some((this, DistinctVariables.empty))
    } else {
      None
    }
  }
  override def findSubstitution(other: Component, termVariable: TermVariable): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    if (this == other) {
      (Seq((termVariable, DistinctVariables.empty)), Some(DistinctVariables(termVariable -> this)))
    } else {
      other match {
        case SubstitutedStatementVariable(variable, termToReplaceWith, `termVariable`) if variable == this =>
          (Seq((termToReplaceWith, DistinctVariables.empty)), None)
        case _ =>
          (Nil, None)
      }
    }
  }
  override def replacePlaceholder(other: Component) = Some(this)
  override def html: String = text
  override def serialized: String = text
}

case class SubstitutedStatementVariable(
    variable: StatementVariable,
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable)
 extends Statement with SubstitutedVariable[Statement, StatementVariable]
{
  override def update(
    updatedVariable: StatementVariable,
    updatedTermToReplaceWith: Term,
    updatedTermToBeReplaced: TermVariable
  ): SubstitutedStatementVariable = {
    SubstitutedStatementVariable(updatedVariable, updatedTermToReplaceWith, updatedTermToBeReplaced)
  }
  override def html: String = "[" + termToReplaceWith.safeHtml + "/" + termToBeReplaced.html + "]" + variable.html
  override def serialized: String = Seq(
    "sub",
    termToReplaceWith.serialized,
    termToBeReplaced.serialized,
    variable.serialized
  ).mkString(" ")
}

case class DefinedStatement(
    subcomponents: Seq[Component],
    localBoundVariables: Set[TermVariable],
    definition: StatementDefinition)
 extends Statement with DefinedComponent[Statement]
{
  override def getMatch(other: Component): Option[(Seq[Component], Set[TermVariable])] = other match {
    case DefinedStatement(otherSubcomponents, otherBoundVariables, `definition`) =>
      Some((otherSubcomponents, otherBoundVariables))
    case _ =>
      None
  }
  override def update(newSubcomponents: Seq[Component], newBoundVariables: Set[TermVariable]): Statement = {
    copy(subcomponents = newSubcomponents, localBoundVariables = newBoundVariables)
  }

  override def html: String = {
    definition.format.html(subcomponents)
  }
  override def safeHtml: String = {
    definition.format.safeHtml(subcomponents)
  }
  override def serialized: String = (definition.symbol +: subcomponents.map(_.serialized)).mkString(" ")
}

object PlaceholderStatement extends Statement with Placeholder[Statement] {
  override def replacePlaceholder(other: Component) = {
    Some(other.asInstanceOf[Statement])
  }
  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[Statement] = {
    Some(SubstitutedPlaceholderStatement(termToReplaceWith, termToBeReplaced))
  }
}

case class SubstitutedPlaceholderStatement(
  termToReplaceWith: Term,
  termToBeReplaced: TermVariable)
extends Statement with Placeholder[Statement] {
  override def replacePlaceholder(other: Component): Option[Statement] = {
    other.asInstanceOf[Statement].makeSingleSubstitution(termToReplaceWith, termToBeReplaced, DistinctVariables.empty)
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
        context.statementVariableNames.find(_ == s).map(StatementVariable)
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
          statement.makeSingleSubstitution(termToReplaceWith, termToBeReplaced, DistinctVariables.empty)
            .getOrElse(throw new Exception("Invalid substitution"))
        }
      case "_" =>
        Parser.constant(PlaceholderStatement)
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
