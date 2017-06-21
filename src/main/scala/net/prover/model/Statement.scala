package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

import scala.collection.immutable.Nil
import scala.util.Try

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
  override def allVariables: Variables = Variables(this)
  override def presentVariables: Variables = Variables(this)
  override def boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = (Set.empty, Set.empty)
  def getPotentiallyIntersectingVariables(variable: Variable): Variables = Variables(this)
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
    substitutions.statements.get(this)
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
    statementVariable: StatementVariable,
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable)
 extends Statement
{
  override def allVariables: Variables = termToReplaceWith.allVariables + statementVariable + termToBeReplaced
  override def presentVariables: Variables = termToReplaceWith.allVariables + statementVariable
  override def boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = termToReplaceWith.boundAndFreeVariables
  def getPotentiallyIntersectingVariables(variable: Variable): Variables = {
    if (variable == termToBeReplaced)
      termToReplaceWith.allVariables
    else
      termToReplaceWith.allVariables + statementVariable
  }
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Seq[PartialSubstitutions] = other match {
    case SubstitutedStatementVariable(otherStatementVariable, otherTermToReplaceWith, otherTermToBeReplaced) =>
      for {
        s1 <- statementVariable.calculateSubstitutions(otherStatementVariable, substitutions)
        s2 <- termToReplaceWith.calculateSubstitutions(otherTermToReplaceWith, s1)
        s3 <- termToBeReplaced.calculateSubstitutions(otherTermToBeReplaced, s2)
      } yield s3
    case statement: Statement =>
      substitutions.tryAdd(this, statement)
    case _ =>
      Nil
  }
  override def applySubstitutions(substitutions: Substitutions): Option[Statement] = {
    for {
      updatedStatementVariable <- statementVariable.applySubstitutions(substitutions)
      updatedTermToReplaceWith <- termToReplaceWith.applySubstitutions(substitutions)
      updatedTermToBeReplaced <- termToBeReplaced.applySubstitutions(substitutions).flatMap(Term.optionAsVariable)
      updatedStatement <- updatedStatementVariable.makeSingleSubstitution(
        updatedTermToReplaceWith,
        updatedTermToBeReplaced,
        substitutions.distinctVariables)
    } yield updatedStatement
  }
  override def makeSingleSubstitution(newTermToReplaceWith: Term, newTermToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[Statement] = {
    if (newTermToReplaceWith == newTermToBeReplaced)
      Some(this)
    else if (newTermToBeReplaced == termToBeReplaced)
      Some(this)
    else if (newTermToBeReplaced == termToReplaceWith && distinctVariables.areDistinct(newTermToBeReplaced, statementVariable))
      Some(SubstitutedStatementVariable(statementVariable, newTermToReplaceWith, termToBeReplaced))
    else
      termToReplaceWith match {
        case termVariableToReplaceWith: TermVariable
          if distinctVariables.areDistinct(newTermToBeReplaced, statementVariable) && distinctVariables.areDistinct(newTermToBeReplaced, termVariableToReplaceWith)
        =>
          Some(this)
        case _ =>
          None
      }
  }
  override def validateSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    target: Component,
    distinctVariables: DistinctVariables
  ): Option[DistinctVariables] = {
    if (makeSingleSubstitution(termToReplaceWith, termToBeReplaced, distinctVariables).contains(target)) {
      Some(DistinctVariables.empty)
    } else {
      None
    }
  }
  override def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[(Statement, DistinctVariables)] = {
    if (other == this) {
      Some((this, DistinctVariables(termVariable -> presentVariables)))
    } else {
      None
    }
  }
  override def findSubstitution(other: Component, termVariable: TermVariable): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    if (this == other) {
      if (presentVariables.termVariables.contains(termVariable))
        (Seq((termVariable, DistinctVariables.empty)), None)
      else if (termVariable == termToBeReplaced)
        (Seq((termVariable, DistinctVariables.empty)), Some(DistinctVariables(termVariable -> termToReplaceWith.presentVariables)))
      else
        (Seq((termVariable, DistinctVariables.empty)), Some(DistinctVariables(termVariable -> presentVariables)))
    } else {
      other match {
        case `statementVariable` if termToReplaceWith == termVariable =>
          (Seq((termToBeReplaced, DistinctVariables(termVariable -> statementVariable))), None)
        case SubstitutedStatementVariable(`statementVariable`, otherTermToReplaceWith: TermVariable, `termToBeReplaced`) if termToReplaceWith == termVariable =>
          (Seq((otherTermToReplaceWith, DistinctVariables(termVariable -> statementVariable))), None)
        case _ =>
          (Nil, None)
      }
    }
  }
  override def replacePlaceholder(other: Component) = Some(this)
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
  override def presentVariables: Variables = subcomponents.map(_.presentVariables).foldLeft(Variables.empty)(_ ++ _)
  override val boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = {
    val (mergedBound, mergedFree) = mergeBoundAndFreeVariables(subcomponents)
    mergedBound.intersect(localBoundVariables).headOption.foreach { v => throw new Exception(s"Variable $v is bound twice in $this")}
    (mergedBound ++ localBoundVariables, mergedFree -- localBoundVariables)
  }
  def getPotentiallyIntersectingVariables(variable: Variable): Variables = {
    variable match {
      case termVariable: TermVariable if localBoundVariables.contains(termVariable) =>
        Variables.empty
      case _ =>
        subcomponents
          .map(_.getPotentiallyIntersectingVariables(variable))
          .foldLeft(Variables.empty)(_ ++ _)
    }
  }

  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Seq[PartialSubstitutions] = other match {
    case DefinedStatement(otherSubcomponents, _, `definition`) =>
      subcomponents.zip(otherSubcomponents)
        .foldLeft(Seq(substitutions)) { case (substitutionsSoFar, (component, otherComponent)) =>
          substitutionsSoFar.flatMap(component.calculateSubstitutions(otherComponent, _))
        }
    case _ =>
      Nil
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

  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[Statement] = {
    for {
      updatedSubcomponents <- subcomponents
        .map(_.makeSingleSubstitution(termToReplaceWith, termToBeReplaced, distinctVariables))
        .traverseOption
    } yield copy(subcomponents = updatedSubcomponents)
  }
  override def validateSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    target: Component,
    distinctVariables: DistinctVariables
  ): Option[DistinctVariables] = {
    target match {
      case DefinedStatement(otherSubcomponents, _, `definition`) =>
        validateSubstitution(termToReplaceWith, termToBeReplaced, subcomponents, otherSubcomponents, distinctVariables)
      case _ =>
        None
    }
  }

  def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[(Statement, DistinctVariables)] = {
    other match {
      case DefinedStatement(otherSubcomponents, otherBoundVariables, `definition`) =>
        if (
          localBoundVariables.contains(termVariable) ||
          thisTerm.presentVariables.termVariables.exists(localBoundVariables.contains) ||
          otherBoundVariables.contains(termVariable) ||
          otherTerm.presentVariables.termVariables.exists(otherBoundVariables.contains)
        )
          None
        else {
          resolveSubstitution(subcomponents, otherSubcomponents, termVariable, thisTerm, otherTerm)
            .map { case (resolvedSubcomponents, resolutionDistinctVariables) =>
              val thisDistinctPairs = for {
                boundVariable <- localBoundVariables
                variable <- thisTerm.presentVariables.termVariables -- localBoundVariables
              } yield boundVariable -> variable
              val otherDistinctPairs = for {
                boundVariable <- otherBoundVariables
                variable <- otherTerm.presentVariables.termVariables -- otherBoundVariables
              } yield boundVariable -> variable
              val additionalDistinctVariables = DistinctVariables((thisDistinctPairs ++ otherDistinctPairs).toSeq :_*)
              (copy(subcomponents = resolvedSubcomponents), resolutionDistinctVariables ++ additionalDistinctVariables)
            }
        }
      case _ =>
        None
    }
  }
  def findSubstitution(other: Component, termVariable: TermVariable): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    other match {
      case DefinedStatement(otherSubcomponents, _, `definition`) =>
        findSubstitution(subcomponents, otherSubcomponents, termVariable)
      case _ =>
        (Nil, None)
    }
  }

  override def replacePlaceholder(other: Component) = {
    for {
      updatedSubcomponents <- subcomponents.map(_.replacePlaceholder(other)).traverseOption
      updated <- Try(copy(subcomponents = updatedSubcomponents)).toOption
    } yield updated
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
