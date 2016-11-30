package net.prover.model

trait Term {
  def variables: Variables
  def attemptMatch(otherTerm: Term): Option[Match]
  def applyMatch(m: Match): Term
  def substituteTermVariables(termToReplaceWith: TermVariable, termToBeReplaced: TermVariable): Term
  def html: String
  override def toString: String = html
}

case class TermVariable(i: Int) extends Term {
  override def variables: Variables = Variables(Nil, Seq(this))
  override def attemptMatch(otherTerm: Term): Option[Match] = {
    Some(Match(Map.empty, Map(this -> otherTerm)))
  }
  override def applyMatch(m: Match): Term = {
    m.terms.getOrElse(this, throw new Exception(s"No replacement for term variable $this"))
  }
  override def substituteTermVariables(termToReplaceWith: TermVariable, termToBeReplaced: TermVariable): TermVariable = {
    if (this == termToBeReplaced)
      termToReplaceWith
    else
      this
  }
  override def html: String = (123 - i).toChar.toString
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
    val (termType, remainingLine) = line.splitFirstWord
    termType match {
      case IntParser(i) =>
        (TermVariable(i), remainingLine)
      case _ =>
        throw ParseException.withMessage(s"Unrecognised term type '$termType'", line.fullLine)
    }
  }
}
