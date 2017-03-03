package net.prover.model

case class Format(formatString: String, replacementNames: Seq[String], requiresBrackets: Boolean) {
  def html(components: Seq[Component]): String = {
    components.zip(replacementNames).foldLeft(formatString) { case (htmlSoFar, (component, replacement)) =>
      htmlSoFar.replaceFirst(replacement, component.safeHtml)
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
    replacementNames: Seq[String]
  ): Format = {
    replacementNames match {
      case Nil =>
        Format(symbol, replacementNames, requiresBrackets = false)
      case Seq(single) =>
        Format(s"$symbol$single", replacementNames, requiresBrackets = false)
      case Seq(first, second) =>
        Format(s"$first $symbol $second", replacementNames, requiresBrackets = true)
      case _ =>
        throw new Exception("Explicit format must be supplied with more than two components")
    }
  }

  def parser(symbol: String, replacementNames: Seq[String]): Parser[Format] = {
    for {
      rawFormat <- Parser.allInParens
    } yield {
      if (rawFormat.endsWith("in parens"))
        Format(rawFormat.stripSuffix("in parens").trim, replacementNames, requiresBrackets = true)
      else
        Format(rawFormat, replacementNames, requiresBrackets = false)
    }
  }

  def optionalParser(symbol: String, replacementNames: Seq[String]): Parser[Format] = {
    Parser.optional(
      "format",
      parser(symbol, replacementNames),
      default(symbol, replacementNames))
  }
}
