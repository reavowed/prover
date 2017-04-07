package net.prover.model

import net.prover.model.Inference.{DirectPremise, Premise}

case class TermDefinition(
    symbol: String,
    defaultVariables: Seq[Component],
    format: Format,
    premises: Seq[Statement],
    placeholderDefinition: Statement)
  extends ChapterEntry(TermDefinition)
{
  val id: String = s"definition-$symbol"
  val defaultTerm = DefinedTerm(defaultVariables, this)
  val componentTypes = defaultVariables.map(_.componentType)
  val definition = placeholderDefinition.replacePlaceholder(defaultTerm).getOrElse(
    throw new Exception(s"Invalid placeholder statement / term combo '$placeholderDefinition' / '$defaultTerm'"))
  val inference: Inference = new Inference {
    override val name: String = s"Definition of $symbol"
    override val premises: Seq[Premise] = TermDefinition.this.premises.map(DirectPremise)
    override val conclusion: ProvenStatement = ProvenStatement.withNoConditions(definition)
  }

  def apply(components: Seq[Component]) = DefinedTerm(components, this)

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

  def parser(implicit context: Context): Parser[TermDefinition] = {
    for {
      symbol <- Parser.singleWord
      defaultVariables <- Component.variableParser.listInParens(None)
      format <- Format.optionalParser(symbol, defaultVariables.map(_.html))
      premises <- premisesParser
      definitionTemplate <- Statement.parser.inParens
    } yield {
      TermDefinition(symbol, defaultVariables, format, premises, definitionTemplate)
    }
  }

  override def addToContext(termDefinition: TermDefinition, context: Context): Context = {
    context.addTermDefinition(termDefinition)
  }
}

