package net.prover.model

object Parser {
  def inParens[T](line: PartialLine, f: PartialLine => (T, PartialLine)): (T, PartialLine) = {
    if (line.remainingText.head != '(') {
      throw ParseException.withMessage("Open-paren expected but not found", line.fullLine)
    }
    val (t, remainingLine) = f(line.tail)
    if (remainingLine.remainingText.head != ')') {
      throw ParseException.withMessage("Close-paren expected but not found", line.fullLine)
    }
    (t, remainingLine.tail)
  }
}
