package net.prover.model

import java.util.regex.{Matcher, Pattern}

import com.fasterxml.jackson.annotation.JsonIgnoreProperties

import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, Node, NodeSeq, Text}

@JsonIgnoreProperties(value = Array("originalValue"))
trait Format {
  def baseFormatString: String
  def requiresBrackets: Boolean
  def serialized: Option[String]

  def formatText(components: Seq[String], safe: Boolean = false): String = {
    components.zipWithIndex.foldLeft(getSafeFormatString(safe)) { case (textSoFar, (component, index)) =>
      textSoFar.replaceFirst(s"%$index", Matcher.quoteReplacement(component))
    }
  }

  private def getSafeFormatString(safe: Boolean) = {
    if (safe && requiresBrackets)
      "(" + baseFormatString + ")"
    else
      baseFormatString
  }
}

object Format {
  case class Default(baseFormatString: String, requiresBrackets: Boolean) extends Format {
    override def serialized = None
  }
  case class Explicit(baseFormatString: String, originalValue: String, requiresBrackets: Boolean) extends Format {
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
