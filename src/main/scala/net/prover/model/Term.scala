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

case class TermVariable(text: String) extends Term with Variable {
  override def allVariables: Variables = Variables(this)
  override def presentVariables: Variables = Variables(this)
  override def boundAndFreeVariables: (Set[TermVariable], Set[TermVariable]) = (Set.empty, Set(this))
  override def implicitDistinctVariables: DistinctVariables = DistinctVariables.empty
  override def getPotentiallyIntersectingVariables(variable: Variable): Variables = Variables(this)
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
  override def findSubstitution(other: Component, termVariable: TermVariable): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    if (this == termVariable) {
      (Seq((other.asInstanceOf[Term], DistinctVariables.empty)), None)
    } else if (this == other) {
      (Nil, Some(DistinctVariables.empty))
    } else {
      (Nil, None)
    }
  }
  override def replacePlaceholder(other: Component) = Some(this)
  override def html: String = Html.format(text)
  override def serialized: String = text
}

case class DefinedTerm(
    subcomponents: Seq[Component],
    localBoundVariables: Set[TermVariable],
    definition: TermDefinition)
  extends Term with DefinedComponent[Term]
{
  override def getMatch(other: Component): Option[(Seq[Component], Set[TermVariable])] = other match {
    case DefinedTerm(otherSubcomponents, otherBoundVariables, `definition`) =>
      Some((otherSubcomponents, otherBoundVariables))
    case _ =>
      None
  }
  override def update(newSubcomponents: Seq[Component], newBoundVariables: Set[TermVariable]): Term = {
    copy(subcomponents = newSubcomponents, localBoundVariables = newBoundVariables)
  }

  override def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[(Term, DistinctVariables)] = {
    if (this == thisTerm && other == otherTerm) {
      Some((termVariable, DistinctVariables.empty))
    } else {
      super.resolveSingleSubstitution(other, termVariable, thisTerm, otherTerm)
    }
  }

  override def html: String = {
    definition.format.html(subcomponents)
  }
  override def safeHtml: String = {
    definition.format.safeHtml(subcomponents)
  }
  override def serialized: String = (definition.symbol +: subcomponents.map(_.serialized)).mkString(" ")
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

  def findVariable(name: String)(implicit context: Context): Option[TermVariable] = {
    def findDirectly(name: String): Option[TermVariable] = {
      context.termVariableNames.find(_ == name).map(TermVariable)
    }
    def findPrime(name: String): Option[TermVariable] = {
      context.termVariableNames.find(_ + "'" == name).map(_ => TermVariable(name))
    }
    def findWithSuffix(name: String): Option[TermVariable] = {
      val index = name.indexOf('_')
      if (index >= 0) {
        val prefix = name.substring(0, index)
        context.termVariableNames.find(_ == prefix).map(_ => TermVariable(name))
      } else {
        None
      }
    }
    findDirectly(name) orElse findPrime(name) orElse findWithSuffix(name)
  }

  def parser(implicit context: Context): Parser[Term] = {
    object TermDefinitionMatcher {
      def unapply(s: String): Option[TermDefinition] = {
        context.termDefinitions.find(_.symbol == s)
      }
    }
    object SpecifiedVariable {
      def unapply(s: String): Option[TermVariable] = {
        findVariable(s)
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
