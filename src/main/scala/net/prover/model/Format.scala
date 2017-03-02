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
  def default(
    symbol: String,
    numberOfComponents: Int
  ): Format = {
    if (numberOfComponents == 0)
      Format(symbol, requiresBrackets = false)
    else if (numberOfComponents == 1)
      Format(s"$symbol{}", requiresBrackets = false)
    else if (numberOfComponents == 2)
      Format(s"{} $symbol {}", requiresBrackets = true)
    else
      throw new Exception("Explicit format must be supplied with more than two components")
  }

  def parser(symbol: String, numberOfComponents: Int): Parser[Format] = {
    for {
      rawFormat <- Parser.allInParens
    } yield {
      if (rawFormat.endsWith("in parens"))
        Format(rawFormat.stripSuffix("in parens").trim, requiresBrackets = true)
      else
        Format(rawFormat, requiresBrackets = false)
    }
  }

  def optionalParser(symbol: String, numberOfComponents: Int): Parser[Format] = {
    Parser.optional(
      "format",
      parser(symbol, numberOfComponents),
      default(symbol, numberOfComponents))
  }
}
