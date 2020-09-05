package net.prover.model

import java.util.regex.Pattern

import net.prover._
import net.prover.model.definitions.ExpressionDefinition.ComponentType

trait Format {
  def requiresBrackets: Boolean
  def baseFormatString: String
  def numberOfReplacements: Int
  def formatText(components: Seq[String], symbol: String, parentRequiresBrackets: Boolean = false): String = {
    formatText(symbol +: components, parentRequiresBrackets)
  }
  private def formatText(replacementStrings: Seq[String], parentRequiresBrackets: Boolean): String = {
    val regex = "%(\\d+)".r
    @scala.annotation.tailrec
    def replacePlaceholdersRecursively(prefix: String, remainingText: String): String = {
      regex.findFirstMatchIn(remainingText) match {
        case Some(m) =>
          replacePlaceholdersRecursively(prefix + remainingText.substring(0, m.start) + replacementStrings(m.group(1).toInt), remainingText.substring(m.end))
        case None =>
          prefix + remainingText
      }
    }
    replacePlaceholdersRecursively("", getSafeFormatString(parentRequiresBrackets))
  }
  private def getSafeFormatString(parentRequiresBrackets: Boolean) = {
    if (parentRequiresBrackets && requiresBrackets)
      "(" + baseFormatString + ")"
    else
      baseFormatString
  }
}

object Format {
  trait Basic extends Format {
    def requiresComponentBrackets: Boolean
    def serialized: Option[String]
  }

  case class Default(baseFormatString: String, numberOfReplacements: Int, requiresBrackets: Boolean) extends Format.Basic {
    override def requiresComponentBrackets = true
    override def serialized: Option[String] = None
  }
  case class Explicit(baseFormatString: String, originalValue: String, numberOfReplacements: Int, requiresBrackets: Boolean, requiresComponentBrackets: Boolean) extends Format.Basic {
    override def serialized: Some[String] = Some(s"format $serializedWithoutPrefix")
    def serializedWithoutPrefix: String = (originalValue.inParens +: (
        Some("requires-brackets").filter(_ => requiresBrackets).toSeq ++
        Some("no-component-brackets").filter(_ => !requiresComponentBrackets).toSeq)
    ).mkString(" ")
  }
  object Explicit {
    def apply(originalValue: String, replacementNames: Seq[String], requiresBrackets: Boolean, requiresComponentBrackets: Boolean): Explicit = {
      val baseFormatString = replacementNames.zipWithIndex.foldLeft(originalValue) { case (str, (name, index)) =>
        str.replaceAll(s"(?<!\\[a-zA-Z])${Pattern.quote(name)}(?![a-zA-Z])", s"%$index")
      }
      Format.Explicit(baseFormatString, originalValue, replacementNames.length, requiresBrackets, requiresComponentBrackets)
    }
  }
  case class Concatenated(first: Format, second: Format) extends Format {
    override val baseFormatString: String = first.baseFormatString + " " + second.formatText((first.numberOfReplacements until first.numberOfReplacements + second.numberOfReplacements).map(i => s"%$i"), false)
    override def requiresBrackets: Boolean = true
    override def numberOfReplacements: Int = first.numberOfReplacements + second.numberOfReplacements
  }

  def default(boundVariableNames: Seq[String], componentTypes: Seq[ComponentType]): Format.Default = {
    if (boundVariableNames.nonEmpty) {
      throw new Exception("Explicit format must be supplied for definition with bound variables")
    }
    default(componentTypes.length)
  }
  def default(numberOfComponents: Int): Format.Default = {
    val (formatString, requiresBrackets) = numberOfComponents match {
      case 0 =>
        ("%0", false)
      case 1 =>
        (s"%0%1", false)
      case 2 =>
        (s"%1 %0 %2", true)
      case _ =>
        throw new Exception("Explicit format must be supplied for definition with more than two components")
    }
    Format.Default(formatString, numberOfComponents + 1, requiresBrackets)
  }

  def optionalParserForExpressionDefinition(symbol: String, boundVariableNames: Seq[String], componentTypes: Seq[ComponentType]): Parser[Format.Basic] = {
    Parser.optional(
      "format",
      parserForExpressionDefinition(symbol, boundVariableNames, componentTypes),
      default(boundVariableNames, componentTypes))
  }

  def parserForExpressionDefinition(symbol: String, boundVariableNames: Seq[String], componentTypes: Seq[ComponentType]): Parser[Format.Explicit] = {
    parser((symbol +: boundVariableNames) ++ componentTypes.map(_.name))
  }

  def parserForTypeDefinition(simpleVariableDefinitions: Seq[SimpleVariableDefinition]): Parser[Format.Explicit] = {
    parser(simpleVariableDefinitions.map(_.name))
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
