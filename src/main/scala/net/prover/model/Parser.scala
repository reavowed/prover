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

  def listInParens[T](line: PartialLine, f: PartialLine => (T, PartialLine), separatorOption: Option[String] = Some(",")): (Seq[T], PartialLine) = {
    def parseRecursive(lineSoFar: PartialLine, acc: Seq[T]): (Seq[T], PartialLine) = {
      if (lineSoFar.remainingText.head == ')') {
        (acc, lineSoFar.tail)
      } else {
        val lineToParse = separatorOption match {
          case Some(separator) if acc.nonEmpty =>
            if (lineSoFar.remainingText.startsWith(separator)) lineSoFar.tail
            else throw ParseException.withMessage("Expected comma or end-paren after list item", line.fullLine)
          case _ =>
            lineSoFar
        }
        val (next, remainingLine) = f(lineToParse)
        parseRecursive(remainingLine, acc :+ next)
      }
    }
    if (line.remainingText.head != '(') {
      throw ParseException.withMessage("Open-paren expected but not found", line.fullLine)
    }
    parseRecursive(line.tail, Nil)
  }

  def parseFormat(line: PartialLine, symbol: String, numberOfComponents: Int): (String, Boolean, PartialLine) = {
    val (rawFormat, remainingLine) = Parser.inParens(line, _.toEndOfParens)
    val (format, requiresBrackets) = rawFormat match {
      case f if f.nonEmpty =>
        if (f.endsWith("in parens"))
          (f.stripSuffix("in parens").trim, true)
        else
          (f, false)
      case "" if numberOfComponents == 2 =>
        (s"{} $symbol {}", true)
      case "" if numberOfComponents == 1 =>
        (s"$symbol {}", false)
      case "" if numberOfComponents == 0 =>
        (symbol, false)
      case "" =>
        throw ParseException.withMessage("Explicit format must be supplied with more than two components", line.fullLine)
    }
    (format, requiresBrackets, remainingLine)
  }
}
