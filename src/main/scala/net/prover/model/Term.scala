package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

trait Term extends JsonSerializable.Base with Component {
  override val componentType = Term
  override def serialize(gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeString(html)
  }
  override def serializeWithType(gen: JsonGenerator, serializers: SerializerProvider, typeSer: TypeSerializer): Unit = {
    serialize(gen, serializers)
  }
  def applySubstitutions(substitutions: Substitutions): Term
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable): Term
  def resolveSingleSubstitution(other: Component, termVariable: TermVariable, thisTerm: Term, otherTerm: Term): Option[Term]
}

case class TermVariable(text: String) extends Term {
  override def variables: Variables = Variables(Set.empty, Set(this))
  override def freeVariables = Set(this)
  override def boundVariables = Set.empty
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
  override def applySubstitutions(substitutions: Substitutions): Term = {
    substitutions.terms.getOrElse(this, {
      throw new Exception(s"No replacement for term variable $this")
    })
  }
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable): Term = {
    if (termToBeReplaced == this)
      termToReplaceWith
    else
      this
  }
  def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[Term] = {
    if (this == thisTerm && other == otherTerm) {
      Some(termVariable)
    } else if (this == other) {
      Some(this)
    } else {
      None
    }
  }
  def findSubstitution(other: Component, termVariable: TermVariable): Option[Option[Term]] = {
    if (this == termVariable) {
      Some(Some(other.asInstanceOf[Term]))
    } else {
      Some(None)
    }
  }
  override def html: String = text
  override def serialized: String = text
}

case class DefinedTerm(
    subcomponents: Seq[Component],
    termSpecification: TermSpecification)
  extends Term
{
  override def variables: Variables = subcomponents.map(_.variables).foldLeft(Variables.empty)(_ ++ _)
  override def freeVariables = subcomponents.flatMap(_.freeVariables).toSet
  override def boundVariables = Set.empty
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Option[PartialSubstitutions] = other match {
    case DefinedTerm(otherSubcomponents, `termSpecification`) =>
      subcomponents.zip(otherSubcomponents)
        .foldLeft(Option(substitutions)) { case (substitutionsSoFarOption, (component, otherComponent)) =>
          substitutionsSoFarOption.flatMap(component.calculateSubstitutions(otherComponent, _))
        }
    case _ =>
      None
  }
  override def applySubstitutions(substitutions: Substitutions): Term = {
    copy(subcomponents = subcomponents.map(_.applySubstitutions(substitutions)))
  }

  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable): Term = {
    copy(subcomponents = subcomponents.map(_.makeSingleSubstitution(termToReplaceWith, termToBeReplaced)))
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
      case DefinedTerm(otherSubcomponents, `termSpecification`) =>
        subcomponents.zip(otherSubcomponents)
          .map { case (subcomponent, otherSubcomponent) =>
            subcomponent.resolveSingleSubstitution(otherSubcomponent, termVariable, thisTerm, otherTerm)
          }
          .traverseOption
          .map { resolvedSubcomponents =>
            copy(subcomponents = resolvedSubcomponents)
          }
    }
  }

  def findSubstitution(other: Component, termVariable: TermVariable): Option[Option[Term]] = {
    other match {
      case DefinedTerm(otherSubcomponents, `termSpecification`) =>
        subcomponents.zip(otherSubcomponents)
          .map {
            case (subcomponent, otherSubcomponent) =>
              subcomponent.findSubstitution(otherSubcomponent, termVariable)
          }
          .traverseOption
          .flatMap {
            _.flatten match {
              case Seq(singleTerm) =>
                Some(Some(singleTerm))
              case Nil =>
                Some(None)
              case _ =>
                None
            }
          }
      case _ =>
        None
    }
  }


  override def html: String = {
    termSpecification.format.html(subcomponents)
  }
  override def safeHtml: String = {
    termSpecification.format.safeHtml(subcomponents)
  }
  override def serialized: String = (termSpecification.symbol +: subcomponents.map(_.serialized)).mkString(" ")

  override def equals(obj: Any): Boolean = obj match {
    case DefinedTerm(`subcomponents`, otherTermDefinition) if otherTermDefinition.symbol == termSpecification.symbol =>
      true
    case _ =>
      false
  }

  override def hashCode(): Int = {
    subcomponents.hashCode * 41 + termSpecification.symbol.hashCode
  }
}

object Term extends ComponentType {
  def asVariable(term: Term): TermVariable = {
    term match {
      case v: TermVariable =>
        v
      case x =>
        throw new Exception(s"Expected term variable, got $x")
    }
  }

  def parser(implicit context: Context): Parser[Term] = {
    object TermSpecificationMatcher {
      def unapply(s: String): Option[TermSpecification] = {
        context.termSpecifications.find(_.symbol == s)
      }
    }
    object SpecifiedVariable {
      def unapply(s: String): Option[TermVariable] = {
        context.variables.termVariables.find(_.text == s)
      }
    }
    def parserForTermType(termType: String): Parser[Term] = {
      termType match {
        case TermSpecificationMatcher(termSpecification) =>
          termSpecification.termParser
        case SpecifiedVariable(variable) =>
          Parser.constant(variable)
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
