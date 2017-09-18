package net.prover.model.entries

import net.prover.model.expressions._
import net.prover.model.{Format, Inference, Parser, ParsingContext}

case class TermDefinition(
    symbol: String,
    defaultVariables: Seq[Variable],
    name: String,
    format: Format,
    premises: Seq[Statement],
    placeholderDefinition: Statement,
    chapterKey: String,
    bookKey: String)
  extends ChapterEntry(TermDefinition)
{
  val id: String = s"definition-$symbol"
  val defaultValue = DefinedTerm(defaultVariables, this)
  val expressionTypes = defaultVariables.map(_.expressionType)
  val definingStatement = placeholderDefinition.replacePlaceholder(defaultValue)
  override def inferences: Seq[Inference] = Seq(Inference.Definition(name, chapterKey, bookKey, premises, definingStatement))

  def apply(components: Expression*): DefinedTerm = DefinedTerm(components, this)

  def termParser(implicit context: ParsingContext): Parser[Term] = {
    expressionTypes.expressionsParser.map(apply)
  }
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
      defaultVariables <- Variable.parser.listInParens(None)
      name <- nameParser.getOrElse(symbol)
      format <- Format.optionalParser(symbol, defaultVariables.map(_.text))
      premises <- premisesParser
      definitionTemplate <- Statement.parser.inParens
    } yield {
      TermDefinition(
        symbol,
        defaultVariables,
        name,
        format,
        premises,
        definitionTemplate,
        chapterKey,
        bookKey)
    }
  }
}

