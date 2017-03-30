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
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable): Option[Statement]
  def resolveSingleSubstitution(other: Component, termVariable: TermVariable, thisTerm: Term, otherTerm: Term): Option[Statement]
  def replacePlaceholder(other: Component): Statement
}

case class StatementVariable(text: String) extends Statement {
  override def allVariables: Variables = Variables(Set(this), Set.empty)
  override def boundVariables: Set[TermVariable] = Set.empty
  def getPotentiallyIntersectingVariables(termVariable: TermVariable): Variables = Variables(Set(this), Set.empty)
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Option[PartialSubstitutions] = other match {
    case otherStatement: Statement =>
      substitutions.tryAdd(this, otherStatement)
    case _ =>
      None
  }
  override def applySubstitutions(substitutions: Substitutions): Option[Statement] = {
    substitutions.statements.get(this)
  }

  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable) = {
    if (termToReplaceWith == termToBeReplaced)
      Some(this)
    else
      Some(SubstitutedStatementVariable(this, termToReplaceWith, termToBeReplaced))
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
  def findSubstitution(other: Component, termVariable: TermVariable): Seq[(Option[Term], Map[TermVariable, Variables])] = {
    if (this == other) {
      Seq(
        (Some(termVariable), Map.empty),
        (None, Map(termVariable -> Variables(Set(this), Set.empty))))
    } else {
      other match {
        case SubstitutedStatementVariable(variable, termToReplaceWith, `termVariable`) if variable == this =>
          Seq((Some(termToReplaceWith), Map.empty))
        case _ =>
          Nil
      }
    }
  }
  def replacePlaceholder(other: Component): Statement = this
  override def html: String = text
  override def serialized: String = text
}

case class SubstitutedStatementVariable(
    statementVariable: StatementVariable,
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable)
 extends Statement
{
  override def allVariables: Variables = termToReplaceWith.allVariables + statementVariable + termToBeReplaced
  override def presentVariables: Variables = allVariables - termToBeReplaced
  override def boundVariables: Set[TermVariable] = termToReplaceWith.boundVariables
  def getPotentiallyIntersectingVariables(termVariable: TermVariable): Variables = {
    if (termVariable == termToBeReplaced)
      termToReplaceWith.allVariables
    else
      termToReplaceWith.allVariables + statementVariable
  }
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Option[PartialSubstitutions] = other match {
    case SubstitutedStatementVariable(otherStatementVariable, otherTermToReplaceWith, otherTermToBeReplaced) =>
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
  override def applySubstitutions(substitutions: Substitutions): Option[Statement] = {
    for {
      updatedStatementVariable <- statementVariable.applySubstitutions(substitutions)
      updatedTermToReplaceWith <- termToReplaceWith.applySubstitutions(substitutions)
      updatedTermToBeReplaced <- termToBeReplaced.applySubstitutions(substitutions).flatMap(Term.optionAsVariable)
      updatedStatement <- updatedStatementVariable.makeSingleSubstitution(updatedTermToReplaceWith, updatedTermToBeReplaced)
    } yield updatedStatement
  }
  override def makeSingleSubstitution(newTermToReplaceWith: Term, newTermToBeReplaced: TermVariable): Option[Statement] = {
    if (newTermToBeReplaced == termToBeReplaced)
      Some(this)
    else
      None
  }
  def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[Statement] = {
    None // TODO: overly conservative
  }
  def findSubstitution(other: Component, termVariable: TermVariable): Seq[(Option[Term], Map[TermVariable, Variables])] = {
    if (this == other) {
      Seq((None, Map.empty))
    } else {
      Nil
    }
  }
  def replacePlaceholder(other: Component): Statement = this
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
    localBoundVariables: Set[TermVariable],
    definition: StatementDefinition)
 extends Statement
{
  override def allVariables: Variables = subcomponents.map(_.allVariables).foldLeft(Variables.empty)(_ ++ _)
  override def boundVariables = localBoundVariables ++ subcomponents.map(_.boundVariables).knownCommonValues
  def getPotentiallyIntersectingVariables(termVariable: TermVariable): Variables = {
    if (localBoundVariables.contains(termVariable))
      Variables.empty
    else
      subcomponents
        .map(_.getPotentiallyIntersectingVariables(termVariable))
        .foldLeft(Variables.empty)(_ ++ _)
  }

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

  override def applySubstitutions(substitutions: Substitutions): Option[Statement] = {
    for {
      updatedSubcomponents <- subcomponents.map(_.applySubstitutions(substitutions)).traverseOption
      updatedBoundVariables <- localBoundVariables
        .map(_.applySubstitutions(substitutions).flatMap(Term.optionAsVariable))
        .traverseOption
    } yield {
      copy(
        subcomponents = updatedSubcomponents,
        localBoundVariables = updatedBoundVariables)
    }
  }

  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable): Option[Statement] = {
    if (localBoundVariables.contains(termToBeReplaced))
      throw new Exception("Cannot substitute for a bound variable")
    if (termToReplaceWith.allVariables.termVariables.intersect(localBoundVariables).nonEmpty)
      throw new Exception("Cannot substitute a new instance of a bound variable")
    for {
      updatedSubcomponents <- subcomponents
        .map(_.makeSingleSubstitution(termToReplaceWith, termToBeReplaced))
        .traverseOption
    } yield copy(subcomponents = updatedSubcomponents)
  }

  def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[Statement] = {
    other match {
      case DefinedStatement(otherSubcomponents, otherBoundVariables, `definition`) =>
        if (localBoundVariables.contains(termVariable) || otherBoundVariables.contains(termVariable))
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
  def findSubstitution(other: Component, termVariable: TermVariable): Seq[(Option[Term], Map[TermVariable, Variables])] = {
    other match {
      case DefinedStatement(otherSubcomponents, _, `definition`) =>
        findSubstitution(subcomponents, otherSubcomponents, termVariable)
      case _ =>
        Nil
    }
  }

  def replacePlaceholder(other: Component): Statement = {
    copy(subcomponents = subcomponents.map(_.replacePlaceholder(other)))
  }

  override def html: String = {
    definition.format.html(subcomponents)
  }
  override def safeHtml: String = {
    definition.format.safeHtml(subcomponents)
  }
  override def serialized: String = (definition.symbol +: subcomponents.map(_.serialized)).mkString(" ")
}

object PlaceholderStatement extends Statement with Placeholder{
  def replacePlaceholder(other: Component): Statement = {
    other.asInstanceOf[Statement]
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
