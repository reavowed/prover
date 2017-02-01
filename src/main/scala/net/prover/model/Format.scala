package net.prover.model

case class Format(formatString: String, requiresBrackets: Boolean) {
  def html(components: Seq[Component]): String = {
    components.foldLeft(formatString) { case (str, component) =>
      str.replaceFirst("\\{\\}", component.safeHtml)
    }
  }

  def safeHtml(components: Seq[Component]) = {
    if (requiresBrackets)
      "(" + html(components) + ")"
    else
      html(components)
  }
}

object Format {

  private def parseRaw(
    rawFormatString: String,
    symbol: String,
    numberOfComponents: Int,
    line: PartialLine
  ): Format = rawFormatString match {
    case f if f.nonEmpty =>
      if (f.endsWith("in parens"))
        Format(f.stripSuffix("in parens").trim, true)
      else
        Format(f, false)
    case "" if numberOfComponents == 2 =>
      Format(s"{} $symbol {}", true)
    case "" if numberOfComponents == 1 =>
      Format(s"$symbol{}", false)
    case "" if numberOfComponents == 0 =>
      Format(symbol, false)
    case "" =>
      throw ParseException.withMessage("Explicit format must be supplied with more than two components", line.fullLine)
  }

  def default(
    symbol: String,
    numberOfComponents: Int
  ): Format = {
    if (numberOfComponents == 0)
      Format(symbol, false)
    else if (numberOfComponents == 1)
      Format(s"$symbol{}", false)
    else if (numberOfComponents == 2)
      Format(s"{} $symbol {}", true)
    else
      throw new Exception("Explicit format must be supplied with more than two components")
  }

  def parser(symbol: String, numberOfComponents: Int): Parser[Format] = {
    Parser.allInParens.mapWithLine { (rawFormat, line) =>
      parseRaw(rawFormat, symbol, numberOfComponents, line)
    }
  }
}
