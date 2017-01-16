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

  def listInParens[T](line: PartialLine, f: PartialLine => (T, PartialLine)): (Seq[T], PartialLine) = {
    def parseRecursive(lineSoFar: PartialLine, acc: Seq[T]): (Seq[T], PartialLine) = {
      if (lineSoFar.remainingText.head == ')') {
        (acc, lineSoFar.tail)
      } else {
        val lineToParse =
          if (acc.nonEmpty && lineSoFar.remainingText.head == ',') lineSoFar.tail
          else if (acc.nonEmpty) throw ParseException.withMessage("Expected comma or end-paren after list item", line.fullLine)
          else lineSoFar
        val (next, remainingLine) = f(lineToParse)
        parseRecursive(remainingLine, acc :+ next)
      }
    }
    if (line.remainingText.head != '(') {
      throw ParseException.withMessage("Open-paren expected but not found", line.fullLine)
    }
    parseRecursive(line.tail, Nil)
  }
}
