package net.prover.model

import net.prover.model.Inference.{DirectPremise, Premise}

case class TermDefinition(
    symbol: String,
    defaultVariables: Seq[Component],
    name: String,
    format: Format,
    premises: Seq[Statement],
    placeholderDefinition: Statement,
    distinctVariables: DistinctVariables)
  extends ChapterEntry(TermDefinition)
{
  val id: String = s"definition-$symbol"
  val defaultTerm = DefinedTerm(defaultVariables, this)
  val componentTypes = defaultVariables.map(_.componentType)
  val definition = placeholderDefinition.replacePlaceholder(defaultTerm).getOrElse(
    throw new Exception(s"Invalid placeholder statement / term combo '$placeholderDefinition' / '$defaultTerm'"))
  val inference: Inference = DerivedInference(
    s"Definition of ${TermDefinition.this.name}",
    TermDefinition.this.premises.map(DirectPremise),
    ProvenStatement(definition, Conditions(Set.empty, distinctVariables)))

  def apply(components: Component*): DefinedTerm = DefinedTerm(components, this)

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
    Parser.allInParens.map(Some.apply),
    None)

  def parser(implicit context: Context): Parser[TermDefinition] = {
    for {
      symbol <- Parser.singleWord
      defaultVariables <- Component.variableParser.listInParens(None)
      name <- nameParser.getOrElse(symbol)
      format <- Format.optionalParser(symbol, defaultVariables.map(_.html))
      premises <- premisesParser
      definitionTemplate <- Statement.parser.inParens
      distinctVariables <- Conditions.distinctVariablesParser
    } yield {
      TermDefinition(symbol, defaultVariables, name, format, premises, definitionTemplate, distinctVariables)
    }
  }

  override def addToContext(termDefinition: TermDefinition, context: Context): Context = {
    context.addTermDefinition(termDefinition)
  }
}

