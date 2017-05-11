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
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[Term]
  def resolveSingleSubstitution(other: Component, termVariable: TermVariable, thisTerm: Term, otherTerm: Term): Option[(Term, DistinctVariables)]
  def replacePlaceholder(other: Component): Option[Term]
}

case class TermVariable(text: String) extends Term {
  override def allVariables: Variables = Variables(Set.empty, Set(this))
  override def presentVariables: Variables = allVariables
  override def boundVariables = Set.empty
  override def getPotentiallyIntersectingVariables(termVariable: TermVariable): Variables = Variables(Set.empty, Set(this))
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Seq[PartialSubstitutions] = {
    other match {
      case otherTerm: Term =>
        substitutions.tryAdd(this, otherTerm).toSeq
      case _ =>
        Nil
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Option[Term] = {
    substitutions.terms.get(this)
  }
  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[Term] = {
    if (termToBeReplaced == this)
      Some(termToReplaceWith)
    else
      Some(this)
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
  ): Option[(Term, DistinctVariables)] = {
    if (this == thisTerm && other == otherTerm) {
      Some((termVariable, DistinctVariables.empty))
    } else if (this == other && this != termVariable) {
      Some((this, DistinctVariables.empty))
    } else {
      None
    }
  }
  override def findSubstitution(other: Component, termVariable: TermVariable): Seq[(Option[Term], DistinctVariables)] = {
    if (this == termVariable) {
      Seq((Some(other.asInstanceOf[Term]), DistinctVariables.empty))
    } else if (this == other) {
      Seq((None, DistinctVariables.empty))
    } else {
      Nil
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
  override def presentVariables: Variables = subcomponents.map(_.presentVariables).foldLeft(Variables.empty)(_ ++ _)
  override def boundVariables = Set.empty // TODO: derive from definition
  def getPotentiallyIntersectingVariables(termVariable: TermVariable): Variables = {
    subcomponents
      .map(_.getPotentiallyIntersectingVariables(termVariable))
      .foldLeft(Variables.empty)(_ ++ _) // TODO: derive from definition (?)
  }
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Seq[PartialSubstitutions] = other match {
    case DefinedTerm(otherSubcomponents, `definition`) =>
      subcomponents.zip(otherSubcomponents)
        .foldLeft(Seq(substitutions)) { case (substitutionsSoFar, (component, otherComponent)) =>
          substitutionsSoFar.flatMap(component.calculateSubstitutions(otherComponent, _))
        }
    case _ =>
      Nil
  }
  override def applySubstitutions(substitutions: Substitutions): Option[Term] = {
    for {
      updatedSubcomponents <- subcomponents.map(_.applySubstitutions(substitutions)).traverseOption
    } yield {
      copy(subcomponents = updatedSubcomponents)
    }
  }

  override def makeSingleSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    distinctVariables: DistinctVariables
  ): Option[Term] = {
    for {
      updatedSubcomponents <- subcomponents
        .map(_.makeSingleSubstitution(termToReplaceWith, termToBeReplaced, distinctVariables))
        .traverseOption
    } yield copy(subcomponents = updatedSubcomponents)
  }
  def validateSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    target: Component,
    distinctVariables: DistinctVariables
  ): Option[DistinctVariables] = {
    target match {
      case DefinedTerm(otherSubcomponents, `definition`) =>
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
  ): Option[(Term, DistinctVariables)] = {
    if (this == thisTerm && other == otherTerm) {
      Some((termVariable, DistinctVariables.empty))
    } else other match {
      case DefinedTerm(otherSubcomponents, `definition`) =>
        resolveSubstitution(subcomponents, otherSubcomponents, termVariable, thisTerm, otherTerm)
          .map(_.mapLeft { resolvedSubcomponents => copy(subcomponents = resolvedSubcomponents)})
      case _ =>
        None
    }
  }

  def findSubstitution(other: Component, termVariable: TermVariable): Seq[(Option[Term], DistinctVariables)] = {
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
