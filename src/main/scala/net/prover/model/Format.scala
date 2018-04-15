package net.prover.model

import java.util.regex.Matcher

case class Format(formatString: String, requiresBrackets: Boolean) {
  def formatHtml(components: Seq[String], safe: Boolean = false): String = {
    formatInternal(HtmlHelper.format(formatString), components, safe)
  }
  def formatText(components: Seq[String], safe: Boolean = false): String = {
    formatInternal(formatString, components, safe)
  }

  private def formatInternal(
    formatStringToUse: String,
    components: Seq[String],
    safe: Boolean = false
  ): String = {
    val inner = components.zipWithIndex.foldLeft(formatStringToUse) { case (textSoFar, (component, index)) =>
      textSoFar.replaceFirst(s"%$index", Matcher.quoteReplacement(component))
    }
    if (safe && requiresBrackets)
      "(" + inner + ")"
    else
      inner
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

  def parser(replacementNames: Seq[String]): Parser[Format] = {
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
      parser(replacementNames),
      default(symbol, replacementNames))
  }
}
