package net.prover.model.entries

import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.expressions._
import net.prover.model.{Format, Inference, Parser, ParsingContext}

case class TermDefinition(
    symbol: String,
    boundVariableNames: Seq[String],
    componentTypes: Seq[ComponentType],
    name: String,
    format: Format,
    premises: Seq[Statement],
    definitionPredicate: Statement,
    chapterKey: String,
    bookKey: String)
  extends ChapterEntry.SelfOutline
    with ExpressionDefinition
{
  val id: String = s"definition-$symbol"
  val defaultValue = {
    DefinedTerm(
      componentTypes.map(_.expression),
      this,
      0)(
      boundVariableNames)
  }
  val definingStatement = definitionPredicate.specify(ArgumentList(Seq(defaultValue), 0))

  def termParser(implicit context: ParsingContext): Parser[Term] = {
    for {
      newBoundVariableNames <- Parser.nWords(boundVariableNames.length)
      components <- componentTypes.map(_.expressionParser(newBoundVariableNames)).traverseParser
    } yield DefinedTerm(components, this, context.parameterDepth)(newBoundVariableNames)
  }

  def templateParser(implicit context: ParsingContext): Parser[Template] = {
    for {
      newBoundVariableNames <- Parser.nWords(boundVariableNames.length)
      components <- componentTypes.map(_.templateParser(newBoundVariableNames)).traverseParser
    } yield Template.DefinedTerm(this, newBoundVariableNames, components)
  }

  override def inferences: Seq[Inference] = Seq(Inference.Definition(name, chapterKey, bookKey, premises, definingStatement))
}

object TermDefinition extends ChapterEntryParser[TermDefinition] {
  override val name: String = "term"

  def premisesParser(implicit context: ParsingContext): Parser[Seq[Statement]] = Parser.optional(
    "premises",
    Statement.listParser,
    Nil)

  def nameParser(implicit context: ParsingContext): Parser[Option[String]] = Parser.optional(
    "name",
    Parser.allInParens)

  def parser(chapterKey: String, bookKey: String)(implicit context: ParsingContext): Parser[TermDefinition] = {
    for {
      symbol <- Parser.singleWord
      boundVariablesAndComponentTypes <- ExpressionDefinition.boundVariablesAndComponentTypesParser
      boundVariables = boundVariablesAndComponentTypes._1
      componentTypes = boundVariablesAndComponentTypes._2
      name <- nameParser.getOrElse(symbol)
      format <- Format.optionalParser(symbol, boundVariables ++ componentTypes.map(_.name))
      premises <- premisesParser
      definitionPredicate <- Statement.parser(context.addParameterList(Seq("_"))).inParens
    } yield {
      TermDefinition(
        symbol,
        boundVariables,
        componentTypes,
        name,
        format,
        premises,
        definitionPredicate,
        chapterKey,
        bookKey)
    }
  }
}

