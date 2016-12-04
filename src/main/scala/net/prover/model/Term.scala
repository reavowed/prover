package net.prover.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.jsontype.TypeSerializer
import com.fasterxml.jackson.databind.{JsonSerializable, SerializerProvider}

trait Term extends JsonSerializable.Base {
  def variables: Variables
  def freeVariables: Seq[TermVariable]
  def attemptMatch(otherTerm: Term): Option[MatchWithSubstitutions]
  def applyMatch(m: Match): Term
  def substituteTermVariables(termToReplaceWith: TermVariable, termToBeReplaced: TermVariable): Term
  def html: String
  override def toString: String = html

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

case class ConstantTerm(symbol: String) extends Term {
  override def variables: Variables = Variables(Nil, Nil)
  override def freeVariables: Seq[TermVariable] = Nil
  override def attemptMatch(otherTerm: Term): Option[MatchWithSubstitutions] = {
    if (otherTerm == this) {
      Some(MatchWithSubstitutions.empty)
    } else {
      None
    }
  }
  override def applyMatch(m: Match): Term = this
  override def substituteTermVariables(termToReplaceWith: TermVariable, termToBeReplaced: TermVariable): Term = this
  override def html: String = symbol
}

trait TermParser {
  def symbol: String
  def parseTerm(line: PartialLine, context: Context): (Term, PartialLine)
}

object Term {
  def asVariable(term: Term): TermVariable = {
    term match {
      case v: TermVariable =>
        v
      case x =>
        throw new Exception(s"Expected term variable, got $x")
    }
  }

  def parse(line: PartialLine, context: Context): (Term, PartialLine) = {
    object ParsableTerm {
      def unapply(s: String): Option[TermParser] = {
        context.constants.find(_.symbol == s)
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
