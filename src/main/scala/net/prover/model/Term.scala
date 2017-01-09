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

case class TermVariable(i: Int, prime: Boolean = false) extends Term {
  override def variables: Variables = Variables(Nil, Seq(this))
  override def freeVariables: Seq[TermVariable] = Seq(this)
  override def calculateSubstitutions(otherTerm: Term): Option[Substitutions] = {
    Some(Substitutions(Map.empty, Map(this -> otherTerm)))
  }
  override def applySubstitutions(substitutions: Substitutions): Term = {
    if (!substitutions.terms.contains(this)) {
      throw new Exception()
    }
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

  override def html: String = (123 - i).toChar.toString + (if (prime) "'" else "")
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

trait TermParser {
  def symbol: String
  def parseTerm(line: PartialLine, context: Context): (Term, PartialLine)
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
    object ParsableTerm {
      def unapply(s: String): Option[TermParser] = {
        context.termParsers.find(_.symbol == s)
      }
    }
    val primeVariableRegex = "(\\d)'".r
    val (termType, remainingLine) = line.splitFirstWord
    termType match {
      case ParsableTerm(termParser) =>
        termParser.parseTerm(remainingLine, context)
      case IntParser(i) =>
        (TermVariable(i), remainingLine)
      case primeVariableRegex(IntParser(i)) =>
        (TermVariable(i, prime = true), remainingLine)
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
