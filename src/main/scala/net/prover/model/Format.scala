package net.prover.model

import java.util.regex.{Matcher, Pattern}

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.entries.ExpressionDefinition.ComponentType

import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, Node, NodeSeq, Text}

trait Format {
  def baseFormatString: String
  def requiresBrackets: Boolean
  def requiresComponentBrackets: Boolean
  def serialized: Option[String]

  def formatText(components: Seq[String], symbol: String, parentRequiresBrackets: Boolean = false): String = {
    (symbol +: components).zipWithIndex.foldLeft(getSafeFormatString(parentRequiresBrackets)) { case (textSoFar, (component, index)) =>
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
        str.replaceAll(s"(?<!\\[a-zA-Z])${Pattern.quote(name)}(?![a-zA-Z])", s"%$index")
      }
      Format.Explicit(baseFormatString, originalValue, requiresBrackets, requiresComponentBrackets)
    }
  }

  def default(boundVariableNames: Seq[String], componentTypes: Seq[ComponentType]): Format = {
    if (boundVariableNames.nonEmpty) {
      throw new Exception("Explicit format must be supplied for definition with bound variables")
    }
    val (formatString, requiresBrackets) = componentTypes match {
      case Nil =>
        ("%0", false)
      case Seq(_) =>
        (s"%0%1", false)
      case Seq(_, _) =>
        (s"%1 %0 %2", true)
      case _ =>
        throw new Exception("Explicit format must be supplied for definition with more than two components")
    }
    Format.Default(formatString, requiresBrackets)
  }

  def optionalParserForExpressionDefinition(symbol: String, boundVariableNames: Seq[String], componentTypes: Seq[ComponentType]): Parser[Format] = {
    Parser.optional(
      "format",
      parserForExpressionDefinition(symbol, boundVariableNames, componentTypes),
      default(boundVariableNames, componentTypes))
  }

  def parserForExpressionDefinition(symbol: String, boundVariableNames: Seq[String], componentTypes: Seq[ComponentType]): Parser[Format.Explicit] = {
    parser((symbol +: boundVariableNames) ++ componentTypes.map(_.name))
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
}
