package net.prover.model

trait Term {
  def html: String
}

case class TermVariable(i: Int) extends Term {
  override def html: String = (123 - i).toChar.toString
}

object Term {
  def parse(line: PartialLine, context: Context): (Term, PartialLine) = {
    val (termType, remainingLine) = line.splitFirstWord
    termType match {
      case IntParser(i) =>
        (TermVariable(i), remainingLine)
      case _ =>
        throw ParseException.withMessage(s"Unrecognised term type $termType", line.fullLine)
    }
  }
}
