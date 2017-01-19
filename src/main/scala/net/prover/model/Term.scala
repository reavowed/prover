package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}
import shapeless.HList

trait Term extends JsonSerializable.Base with Component[Term] {
  override def serialize(gen: JsonGenerator, serializers: SerializerProvider): Unit = {
    gen.writeString(html)
  }
  override def serializeWithType(gen: JsonGenerator, serializers: SerializerProvider, typeSer: TypeSerializer): Unit = {
    serialize(gen, serializers)
  }
}

case class TermVariable(text: String) extends Term {
  override def variables: Variables = Variables(Nil, Seq(this))
  override def freeVariables: Seq[TermVariable] = Seq(this)
  override def calculateSubstitutions(otherTerm: Term): Option[Substitutions] = {
    Some(Substitutions(Map.empty, Map(this -> otherTerm)))
  }
  override def applySubstitutions(substitutions: Substitutions): Term = {
    substitutions.terms.getOrElse(this, {
      throw new Exception(s"No replacement for term variable $this")
    })
  }
  override def substituteFreeVariable(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): Term = {
    if (this == termToBeReplaced)
      termToReplaceWith
    else
      this
  }

  override def attemptSimplification(other: Term): Option[DistinctVariables] = {
    if (other == this)
      Some(DistinctVariables.empty)
    else
      None
  }

  override def makeSimplifications(distinctVariables: DistinctVariables): Term = {
    this
  }

  override def html: String = text
}

case class DefinedTerm[Components <: HList](
    components: Components,
    termDefinition: TermSpecification[Components])
  extends Term
{
  override def variables: Variables = termDefinition.componentTypes.getVariables(components)
  override def freeVariables: Seq[TermVariable] = termDefinition.componentTypes.getFreeVariables(components)
  override def calculateSubstitutions(otherTerm: Term): Option[Substitutions] = otherTerm match {
    case termDefinition(otherComponents) =>
      termDefinition.componentTypes.calculateSubstitutions(components, otherComponents)
    case _ =>
      None
  }
  override def applySubstitutions(substitutions: Substitutions): Term = {
    termDefinition(termDefinition.componentTypes.applySubstitutions(components, substitutions))
  }
  override def substituteFreeVariable(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): Term = {
    termDefinition(termDefinition.componentTypes.substituteFreeVariable(
      components,
      termToReplaceWith,
      termToBeReplaced))
  }
  override def attemptSimplification(otherTerm: Term): Option[DistinctVariables] = otherTerm match {
    case termDefinition(otherComponents) =>
      termDefinition.componentTypes.attemptSimplification(components, otherComponents)
    case _ =>
      None
  }
  override def makeSimplifications(distinctVariables: DistinctVariables): Term = {
    termDefinition(termDefinition.componentTypes.makeSimplifications(components, distinctVariables))
  }
  override def html: String = {
    termDefinition.componentTypes.format(termDefinition.format, components)
  }

  override def safeHtml: String = {
    if (termDefinition.requiresBrackets)
      s"($html)"
    else
      html
  }

  override def equals(obj: Any): Boolean = obj match {
    case DefinedTerm(`components`, otherTermDefinition) if otherTermDefinition.symbol == termDefinition.symbol =>
      true
    case _ =>
      false
  }

  override def hashCode(): Int = {
    components.hashCode * 41 + termDefinition.symbol.hashCode
  }
}

object Term extends ComponentType[Term] {
  def asVariable(term: Term): TermVariable = {
    term match {
      case v: TermVariable =>
        v
      case x =>
        throw new Exception(s"Expected term variable, got $x")
    }
  }

  override def parse(line: PartialLine, context: Context): (Term, PartialLine) = {
    object TermSpecificationMatcher {
      def unapply(s: String): Option[TermSpecification[_]] = {
        context.termSpecifications.find(_.symbol == s)
      }
    }
    object SpecifiedVariable {
      def unapply(s: String): Option[TermVariable] = {
        context.variables.termVariables.find(_.text == s)
      }
    }
    val primeVariableRegex = "(\\d)'".r
    val (termType, remainingLine) = line.splitFirstWord
    termType match {
      case TermSpecificationMatcher(termSpecification) =>
        termSpecification.parseTerm(remainingLine, context)
      case SpecifiedVariable(variable) =>
        (variable, remainingLine)
      case IntParser(i) =>
        (TermVariable((123 - i).toChar.toString), remainingLine)
      case primeVariableRegex(IntParser(i)) =>
        (TermVariable((123 - i).toChar.toString + "'"), remainingLine)
      case _ =>
        throw ParseException.withMessage(s"Unrecognised term type '$termType'", line.fullLine)
    }
  }

  def parseList(line: PartialLine, context: Context, termsSoFar: Seq[Term] = Nil): (Seq[Term], PartialLine) = {
    val (term, lineAfterTerm) = parse(line, context)
    lineAfterTerm match {
      case WordAndRemainingText("&", remainingText) =>
        parseList(remainingText, context, termsSoFar :+ term)
      case _ =>
        (termsSoFar :+ term, lineAfterTerm)
    }
  }
}
