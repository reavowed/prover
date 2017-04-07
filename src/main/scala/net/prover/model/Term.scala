package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

import scala.collection.immutable.Nil

trait Term extends JsonSerializable.Base with Component {
  override val componentType = Term
  override def serialize(gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeString(html)
  }
  override def serializeWithType(gen: JsonGenerator, serializers: SerializerProvider, typeSer: TypeSerializer): Unit = {
    serialize(gen, serializers)
  }
  def applySubstitutions(substitutions: Substitutions): Option[Term]
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable): Option[Term]
  def resolveSingleSubstitution(other: Component, termVariable: TermVariable, thisTerm: Term, otherTerm: Term): Option[Term]
  def replacePlaceholder(other: Component): Option[Term]
}

case class TermVariable(text: String) extends Term {
  override def allVariables: Variables = Variables(Set.empty, Set(this))
  override def boundVariables = Set.empty
  def getPotentiallyIntersectingVariables(termVariable: TermVariable): Variables = Variables(Set.empty, Set(this))
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Option[PartialSubstitutions] = {
    other match {
      case otherTerm: Term =>
        substitutions.tryAdd(this, otherTerm)
      case _ =>
        None
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Option[Term] = {
    substitutions.terms.get(this)
  }
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable): Option[Term] = {
    if (termToBeReplaced == this)
      Some(termToReplaceWith)
    else
      Some(this)
  }
  def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[Term] = {
    if (this == thisTerm && other == otherTerm) {
      Some(termVariable)
    } else if (this == other && this != termVariable) {
      Some(this)
    } else {
      None
    }
  }
  def findSubstitution(other: Component, termVariable: TermVariable): Seq[(Option[Term], Map[TermVariable, Variables])] = {
    if (this == termVariable) {
      Seq((Some(other.asInstanceOf[Term]), Map.empty))
    } else {
      Seq((None, Map.empty))
    }
  }
  override def replacePlaceholder(other: Component) = Some(this)
  override def html: String = text
  override def serialized: String = text
}

case class DefinedTerm(
    subcomponents: Seq[Component],
    definition: TermDefinition)
  extends Term
{
  override def allVariables: Variables = subcomponents.map(_.allVariables).foldLeft(Variables.empty)(_ ++ _)
  override def boundVariables = Set.empty // TODO: derive from definition
  def getPotentiallyIntersectingVariables(termVariable: TermVariable): Variables = {
    subcomponents
      .map(_.getPotentiallyIntersectingVariables(termVariable))
      .foldLeft(Variables.empty)(_ ++ _) // TODO: derive from definition (?)
  }
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Option[PartialSubstitutions] = other match {
    case DefinedTerm(otherSubcomponents, `definition`) =>
      subcomponents.zip(otherSubcomponents)
        .foldLeft(Option(substitutions)) { case (substitutionsSoFarOption, (component, otherComponent)) =>
          substitutionsSoFarOption.flatMap(component.calculateSubstitutions(otherComponent, _))
        }
    case _ =>
      None
  }
  override def applySubstitutions(substitutions: Substitutions): Option[Term] = {
    for {
      updatedSubcomponents <- subcomponents.map(_.applySubstitutions(substitutions)).traverseOption
    } yield {
      copy(subcomponents = updatedSubcomponents)
    }
  }

  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable): Option[Term] = {
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
  ): Option[Term] = {
    if (this == thisTerm && other == otherTerm) {
      Some(termVariable)
    } else other match {
      case DefinedTerm(otherSubcomponents, `definition`) =>
        subcomponents.zip(otherSubcomponents)
          .map { case (subcomponent, otherSubcomponent) =>
            subcomponent.resolveSingleSubstitution(otherSubcomponent, termVariable, thisTerm, otherTerm)
          }
          .traverseOption
          .map { resolvedSubcomponents =>
            copy(subcomponents = resolvedSubcomponents)
          }
      case _ =>
        None
    }
  }

  def findSubstitution(other: Component, termVariable: TermVariable): Seq[(Option[Term], Map[TermVariable, Variables])] = {
    other match {
      case DefinedTerm(otherSubcomponents, `definition`) =>
        findSubstitution(subcomponents, otherSubcomponents, termVariable)
      case _ =>
        Nil
    }
  }

  override def replacePlaceholder(other: Component) = {
    for {
      updatedSubcomponents <- subcomponents.map(_.replacePlaceholder(other)).traverseOption
    } yield copy(subcomponents = updatedSubcomponents)
  }

  override def html: String = {
    definition.format.html(subcomponents)
  }
  override def safeHtml: String = {
    definition.format.safeHtml(subcomponents)
  }
  override def serialized: String = (definition.symbol +: subcomponents.map(_.serialized)).mkString(" ")

  override def equals(obj: Any): Boolean = obj match {
    case DefinedTerm(`subcomponents`, otherTermDefinition) if otherTermDefinition.symbol == definition.symbol =>
      true
    case _ =>
      false
  }

  override def hashCode(): Int = {
    subcomponents.hashCode * 41 + definition.symbol.hashCode
  }
}

object PlaceholderTerm extends Term with Placeholder[Term] {
  def replacePlaceholder(other: Component) = {
    Some(other.asInstanceOf[Term])
  }
}

object Term extends ComponentType {
  def asVariable(term: Term): TermVariable = {
    optionAsVariable(term).getOrElse(throw new Exception(s"Expected term variable, got $term"))
  }

  def optionAsVariable(term: Term): Option[TermVariable] = {
    term match {
      case v: TermVariable =>
        Some(v)
      case _ =>
        None
    }
  }

  def parser(implicit context: Context): Parser[Term] = {
    object TermDefinitionMatcher {
      def unapply(s: String): Option[TermDefinition] = {
        context.termDefinitions.find(_.symbol == s)
      }
    }
    object SpecifiedVariable {
      def unapply(s: String): Option[TermVariable] = {
        context.variables.termVariables.find(_.text == s)
      }
    }
    def parserForTermType(termType: String): Parser[Term] = {
      termType match {
        case TermDefinitionMatcher(termDefinition) =>
          termDefinition.termParser
        case SpecifiedVariable(variable) =>
          Parser.constant(variable)
        case "_" =>
          Parser.constant(PlaceholderTerm)
        case _ =>
          throw new Exception(s"Unrecognised term type '$termType'")
      }
    }
    Parser.singleWord.flatMap(parserForTermType)
  }

  def listParser(implicit context: Context): Parser[Seq[Term]] = {
    parser.listInParens(Some(","))
  }

  def variableParser(implicit context: Context): Parser[TermVariable] = parser.map(asVariable)

  def variableListParser(implicit context: Context): Parser[Seq[TermVariable]] = {
    variableParser.listInParens(None)
  }
}
