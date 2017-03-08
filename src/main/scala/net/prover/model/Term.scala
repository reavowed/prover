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
}

case class TermVariable(text: String) extends Term {
  override def variables: Variables = Variables(Nil, Seq(this))
  override def calculateSubstitutions(other: Component, substitutions: Substitutions): Option[Substitutions] = {
    other match {
      case otherTerm: Term =>
        substitutions.terms.get(this) match {
          case Some(`otherTerm`) =>
            Some(substitutions)
          case Some(_) =>
            None
          case None =>
            Some(substitutions + (this, otherTerm))
        }
      case _ =>
        None
    }
  }
  override def applySubstitutions(substitutions: Substitutions): Term = {
    substitutions.terms.getOrElse(this, {
      throw new Exception(s"No replacement for term variable $this")
    })
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
  override def calculateSubstitutions(other: Component, substitutions: Substitutions): Option[Substitutions] = other match {
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
    val primeVariableRegex = "(\\d)'".r
    def parserForTermType(termType: String): Parser[Term] = {
      termType match {
        case TermSpecificationMatcher(termSpecification) =>
          termSpecification.termParser
        case SpecifiedVariable(variable) =>
          Parser.constant(variable)
        case IntParser(i) =>
          Parser.constant(TermVariable((123 - i).toChar.toString))
        case primeVariableRegex(IntParser(i)) =>
          Parser.constant(TermVariable((123 - i).toChar.toString + "'"))
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
