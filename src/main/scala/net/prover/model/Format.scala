package net.prover.model

import java.util.regex.{Matcher, Pattern}

import com.fasterxml.jackson.annotation.JsonIgnoreProperties

import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, Node, NodeSeq, Text}

trait Format {
  def baseFormatString: String
  def requiresBrackets: Boolean
  def requiresComponentBrackets: Boolean
  def serialized: Option[String]

  def formatText(components: Seq[String], parentRequiresBrackets: Boolean = false): String = {
    components.zipWithIndex.foldLeft(getSafeFormatString(parentRequiresBrackets)) { case (textSoFar, (component, index)) =>
      textSoFar.replaceFirst(s"%$index", Matcher.quoteReplacement(component))
    }
  }

  private def getSafeFormatString(parentRequiresBrackets: Boolean) = {
    if (parentRequiresBrackets && requiresBrackets)
      "(" + baseFormatString + ")"
    else
      baseFormatString
  }
}

object Format {
  case class Default(baseFormatString: String, requiresBrackets: Boolean) extends Format {
    override def requiresComponentBrackets = true
    override def serialized = None
  }
  case class Explicit(baseFormatString: String, originalValue: String, requiresBrackets: Boolean, requiresComponentBrackets: Boolean) extends Format {
    override def serialized = Some(s"format $serializedWithoutPrefix")
    def serializedWithoutPrefix = (originalValue.inParens +: (
        Some("requires-brackets").filter(_ => requiresBrackets).toSeq ++
        Some("no-component-brackets").filter(_ => !requiresComponentBrackets).toSeq)
    ).mkString(" ")
  }
  object Explicit {
    def apply(originalValue: String, replacementNames: Seq[String], requiresBrackets: Boolean, requiresComponentBrackets: Boolean): Explicit = {
      val baseFormatString = replacementNames.zipWithIndex.foldLeft(originalValue) { case (str, (name, index)) =>
        str.replaceAll(name, s"%$index")
      }
      Format.Explicit(baseFormatString, originalValue, requiresBrackets, requiresComponentBrackets)
    }
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
      requiresBrackets <- Parser.optionalWord("requires-brackets").isDefined
      noComponentBrackets <- Parser.optionalWord("no-component-brackets").isDefined
    } yield {
      Format.Explicit(originalString, replacementNames, requiresBrackets, !noComponentBrackets)
    }
  }

  def optionalParser(symbol: String, replacementNames: Seq[String]): Parser[Format] = {
    Parser.optional(
      "format",
      parser(replacementNames),
      default(symbol, replacementNames))
  }
}
