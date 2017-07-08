package net.prover.model.entries

import net.prover.model.components._
import net.prover.model.{Book, Chapter, ChapterEntry, Conditions, Context, DefinitionInference, DistinctVariables, Format, Inference, Parser}

case class TermDefinition(
    symbol: String,
    defaultVariables: Seq[Component],
    name: String,
    format: Format,
    premises: Seq[Statement],
    placeholderDefinition: Statement,
    boundVariables: Set[TermVariable],
    distinctVariables: DistinctVariables)
  extends ChapterEntry(TermDefinition)
{
  val id: String = s"definition-$symbol"
  val defaultValue = DefinedTerm(defaultVariables, boundVariables, this)
  val componentTypes = defaultVariables.map(_.componentType)
  val definingStatement = placeholderDefinition.replacePlaceholder(defaultValue).getOrElse(
    throw new Exception(s"Invalid placeholder statement / term combo '$placeholderDefinition' / '$defaultValue'"))
  val inference: Inference = DefinitionInference(
    name,
    premises,
    definingStatement,
    distinctVariables)

  def apply(components: Component*): DefinedTerm = DefinedTerm(
    components,
    boundVariables.map { v =>
      components(defaultVariables.indexOf(v))
    }.map(_.asInstanceOf[Term]).map(Term.asVariable), this)

  def termParser(implicit context: Context): Parser[Term] = {
    componentTypes.componentsParser.map(apply)
  }
}

object TermDefinition extends ChapterEntryParser[TermDefinition] {
  override val name: String = "term"

  def premisesParser(implicit context: Context): Parser[Seq[Statement]] = Parser.optional(
    "premises",
    Statement.listParser,
    Nil)

  def nameParser(implicit context: Context): Parser[Option[String]] = Parser.optional(
    "name",
    Parser.allInParens)

  def parser(book: Book, chapter: Chapter)(implicit context: Context): Parser[TermDefinition] = {
    for {
      symbol <- Parser.singleWord
      defaultVariables <- Variable.parser.listInParens(None)
      name <- nameParser.getOrElse(symbol)
      format <- Format.optionalParser(symbol, defaultVariables.map(_.html))
      premises <- premisesParser
      definitionTemplate <- Statement.parser.inParens
      boundVariables = defaultVariables.ofType[TermVariable].toSet.filter { v =>
        definitionTemplate.boundVariables.contains(v) || !definitionTemplate.presentVariables.contains(v)
      }
      distinctVariables <- Conditions.distinctVariablesParser
    } yield {
      TermDefinition(symbol, defaultVariables, name, format, premises, definitionTemplate, boundVariables, distinctVariables)
    }
  }

  override def addToContext(termDefinition: TermDefinition, context: Context): Context = {
    context.addTermDefinition(termDefinition)
  }
}

