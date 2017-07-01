package net.prover.model

case class Format(formatString: String, requiresBrackets: Boolean) {
  def html(components: Seq[Component]): String = {
    val replacedText = components.zipWithIndex.foldLeft(formatString) { case (htmlSoFar, (component, index)) =>
      htmlSoFar.replaceFirst(s"%$index", component.safeHtml)
    }
    Html.format(replacedText)
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
        Format(symbol, requiresBrackets = false)
      case Seq(_) =>
        Format(s"$symbol%0", requiresBrackets = false)
      case Seq(_, _) =>
        Format(s"%0 $symbol %1", requiresBrackets = true)
      case _ =>
        throw new Exception("Explicit format must be supplied with more than two components")
    }
  }

  def parser(symbol: String, replacementNames: Seq[String]): Parser[Format] = {
    for {
      rawFormatWithParens <- Parser.allInParens
    } yield {
      val (rawFormat, requiresBrackets) = if (rawFormatWithParens.endsWith("in parens"))
        (rawFormatWithParens.stripSuffix("in parens").trim, true)
      else
        (rawFormatWithParens, false)
      val replacedFormat = replacementNames.zipWithIndex.foldLeft(rawFormat) { case (str, (name, index)) =>
        str.replaceAll(name, s"%$index")
      }
      Format(replacedFormat, requiresBrackets)
    }
  }

  def optionalParser(symbol: String, replacementNames: Seq[String]): Parser[Format] = {
    Parser.optional(
      "format",
      parser(symbol, replacementNames),
      default(symbol, replacementNames))
  }
}
