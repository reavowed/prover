package net.prover.model

import java.util.regex.Matcher

trait Format {
  def formatString: String
  def requiresBrackets: Boolean
  def serialized: Option[String]

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
  case class Default(formatString: String, requiresBrackets: Boolean) extends Format {
    override def serialized = None
  }
  case class Explicit(formatString: String, originalValue: String, requiresBrackets: Boolean) extends Format {
    override def serialized = Some(originalValue)
  }

  def default(
    symbol: String,
    replacementNames: Seq[String]
  ): Format = {
    val (formatString, requiresBrackets) = replacementNames match {
      case Nil =>
        (symbol, false)
      case Seq(a) =>
        (s"$symbol%0", false)
      case Seq(a, b) =>
        (s"%0 $symbol %1", true)
      case _ =>
        throw new Exception("Explicit format must be supplied with more than two components")
    }
    Format.Default(formatString, requiresBrackets)
  }

  def parser(replacementNames: Seq[String]): Parser[Format.Explicit] = {
    for {
      originalString <- Parser.allInParens
    } yield {
      val (rawString, requiresBrackets) = if (originalString.endsWith("in parens"))
        (originalString.stripSuffix("in parens").trim, true)
      else
        (originalString, false)
      val replacedFormat = replacementNames.zipWithIndex.foldLeft(rawString) { case (str, (name, index)) =>
        str.replaceAll(name, s"%$index")
      }
      Format.Explicit(replacedFormat, originalString, requiresBrackets)
    }
  }

  def optionalParser(symbol: String, replacementNames: Seq[String]): Parser[Format] = {
    Parser.optional(
      "format",
      parser(replacementNames),
      default(symbol, replacementNames))
  }
}
