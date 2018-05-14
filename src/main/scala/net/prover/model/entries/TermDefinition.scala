package net.prover.model.entries

import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.expressions._
import net.prover.model._

case class TermDefinition(
    symbol: String,
    boundVariableNames: Seq[String],
    componentTypes: Seq[ComponentType],
    explicitName: Option[String],
    format: Format,
    premises: Seq[Statement],
    definitionPredicate: Statement,
    chapterTitle: String,
    bookTitle: String)
  extends ChapterEntry
    with ExpressionDefinition
{
  def name = explicitName.getOrElse(symbol)
  def chapterKey = chapterTitle.formatAsKey
  def bookKey = bookTitle.formatAsKey

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

  override def inferences: Seq[Inference] = Seq(Inference.Definition(name, chapterKey, bookKey, premises, definingStatement))

  override def serializedLines: Seq[String] = Seq(s"term $symbol $serializedComponents") ++
    (explicitName.map(n => s"name ($n)").toSeq ++
      format.serialized.map(f => s"format ($f)").toSeq ++
      (if (premises.nonEmpty) Seq(s"premises (${premises.map(_.serialized).mkString(", ")})") else Nil) ++
      Seq("(" + definitionPredicate.serialized + ")")).indent
}

object TermDefinition extends ChapterEntryParser.WithoutKey {
  override val name: String = "term"

  def premisesParser(implicit context: ParsingContext): Parser[Seq[Statement]] = Parser.optional(
    "premises",
    Statement.listParser,
    Nil)

  def nameParser(implicit context: ParsingContext): Parser[Option[String]] = Parser.optional(
    "name",
    Parser.allInParens)

  def parser(chapterTitle: String, bookTitle: String)(implicit context: ParsingContext): Parser[TermDefinition] = {
    for {
      symbol <- Parser.singleWord
      boundVariablesAndComponentTypes <- ExpressionDefinition.boundVariablesAndComponentTypesParser
      boundVariables = boundVariablesAndComponentTypes._1
      componentTypes = boundVariablesAndComponentTypes._2
      name <- nameParser
      format <- Format.optionalParser(symbol, boundVariables ++ componentTypes.map(_.name))
      premises <- premisesParser
      definitionPredicate <- Statement.parser(context.addParameters("_")).inParens
    } yield {
      TermDefinition(
        symbol,
        boundVariables,
        componentTypes,
        name,
        format,
        premises,
        definitionPredicate,
        chapterTitle,
        bookTitle)
    }
  }
}

