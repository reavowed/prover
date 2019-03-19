package net.prover.model.entries

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.expressions._
import net.prover.model._

@JsonIgnoreProperties(Array("defaultValue", "definingStatement"))
case class TermDefinition(
    symbol: String,
    key: ChapterEntry.Key.Anchor,
    boundVariableNames: Seq[String],
    componentTypes: Seq[ComponentType],
    explicitName: Option[String],
    format: Format,
    premises: Seq[Statement],
    definitionPredicate: Statement,
    shorthand: Option[String])
  extends ExpressionDefinition with TypedExpressionDefinition[TermDefinition]
{
  def name = explicitName.getOrElse(symbol)

  val defaultValue = {
    DefinedTerm(componentTypes.map(_.expression), this)(boundVariableNames)
  }
  val definingStatement = definitionPredicate.specify(Seq(defaultValue), 0, 0)

  def termParser(implicit context: ParsingContext): Parser[Term] = {
    componentExpressionParser.map { case (newBoundVariableNames, components) =>
      DefinedTerm(components, this)(newBoundVariableNames)
    }
  }

  def templateParser(implicit context: ParsingContext): Parser[Template] = {
    componentTemplateParser.map { case (newBoundVariableNames, components) =>
      Template.DefinedTerm(this, newBoundVariableNames, components)
    }
  }

  override def withShorthand(newShorthand: Option[String]) = copy(shorthand = newShorthand)

  override def inferences: Seq[Inference] = Seq(Inference.Definition(name, key, premises, definingStatement))

  override def serializedLines: Seq[String] = Seq(s"term $symbol $serializedComponents") ++
    (explicitName.map(n => s"name ($n)").toSeq ++
      format.serialized.map(f => s"format ($f)").toSeq ++
      (if (premises.nonEmpty) Seq(s"premises (${premises.map(_.serialized).mkString(", ")})") else Nil) ++
      Seq("(" + definitionPredicate.serialized + ")") ++
      shorthand.map(s => s"shorthand ($s)").toSeq).indent
}

object TermDefinition extends ChapterEntryParser {
  override val name: String = "term"

  def premisesParser(implicit context: ParsingContext): Parser[Seq[Statement]] = Parser.optional(
    "premises",
    Statement.listParser,
    Nil)

  def nameParser(implicit context: ParsingContext): Parser[Option[String]] = Parser.optional(
    "name",
    Parser.allInParens)

  def parser(getKey: String => (String, Chapter.Key))(implicit context: ParsingContext): Parser[TermDefinition] = {
    for {
      symbol <- Parser.singleWord
      boundVariablesAndComponentTypes <- ExpressionDefinition.boundVariablesAndComponentTypesParser
      boundVariables = boundVariablesAndComponentTypes._1
      componentTypes = boundVariablesAndComponentTypes._2
      name <- nameParser
      format <- Format.optionalParser(symbol, boundVariables ++ componentTypes.map(_.name))
      premises <- premisesParser
      definitionPredicate <- Statement.parser(context.addParameters("_")).inParens
      shorthand <- ExpressionDefinition.shorthandParser
    } yield {
      TermDefinition(
        symbol,
        (ChapterEntry.Key.Anchor.apply _).tupled(getKey(symbol)),
        boundVariables,
        componentTypes,
        name,
        format,
        premises,
        definitionPredicate,
        shorthand)
    }
  }
}

