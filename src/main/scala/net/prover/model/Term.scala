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

case class TermVariable(i: Int) extends Term {
  override def variables: Variables = Variables(Nil, Seq(this))
  override def freeVariables: Seq[TermVariable] = Seq(this)
  override def attemptMatch(otherTerm: Term): Option[MatchWithSubstitutions] = {
    Some(MatchWithSubstitutions(Map.empty, Map(this -> otherTerm), Nil))
  }
  override def applyMatch(m: Match): Term = {
    m.terms.getOrElse(
      this,
      throw new Exception(s"No replacement for term variable $this"))
  }
  override def substituteTermVariables(termToReplaceWith: TermVariable, termToBeReplaced: TermVariable): TermVariable = {
    if (this == termToBeReplaced)
      termToReplaceWith
    else
      this
  }
  override def html: String = (123 - i).toChar.toString
}

case class DefinedTerm[Components <: HList](
    components: Components,
    termDefinition: TermDefinition[Components])
  extends Term
{
  override def variables: Variables = Variables(Nil, Nil)
  override def freeVariables: Seq[TermVariable] = Nil
  override def attemptMatch(otherTerm: Term): Option[MatchWithSubstitutions] = otherTerm match {
    case termDefinition(otherComponents) =>
      termDefinition.componentTypes.attemptMatch(components, otherComponents)
    case _ =>
      None
  }
  override def applyMatch(m: Match): Term = termDefinition(termDefinition.componentTypes.applyMatch(components, m))
  override def substituteTermVariables(termToReplaceWith: TermVariable, termToBeReplaced: TermVariable): Term = {
    termDefinition(termDefinition.componentTypes.substituteTermVariables(
      components,
      termToReplaceWith,
      termToBeReplaced))
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
        context.termDefinitions.find(_.symbol == s)
      }
    }

    val (termType, remainingLine) = line.splitFirstWord
    termType match {
      case ParsableTerm(termParser) =>
        termParser.parseTerm(remainingLine, context)
      case IntParser(i) =>
        (TermVariable(i), remainingLine)
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
