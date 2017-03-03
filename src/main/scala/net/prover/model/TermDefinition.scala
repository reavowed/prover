package net.prover.model

case class TermSpecification(
    symbol: String,
    componentTypes: Seq[ComponentType],
    format: Format)
{
  def apply(components: Seq[Component]) = DefinedTerm(components, this)

  def termParser(implicit context: Context): Parser[Term] = {
    componentTypes.componentsParser.map(apply)
  }
}

case class TermDefinition(
    specification: TermSpecification,
    premises: Seq[Statement],
    defaultVariables: Seq[Component],
    definition: Statement)
  extends ChapterEntry(TermDefinition)
{
  val id: String = s"definition-${specification.symbol}"
  val defaultTerm = DefinedTerm(defaultVariables, specification)
  val inference: Inference = new Inference {
    override val id: String = TermDefinition.this.id
    override val assumption = None
    override val premises: Seq[Statement] = TermDefinition.this.premises
    override val conclusion: Statement = definition
    override val arbitraryVariables: Seq[TermVariable] = Nil
    override val distinctVariables: DistinctVariables = DistinctVariables.empty
  }
}

object TermDefinition extends SingleLineChapterEntryParser[TermDefinition] {
  override val name: String = "term"

  def premisesParser(implicit context: Context): Parser[Seq[Statement]] = Parser.optional(
    "premises",
    Statement.listParser,
    Nil)

  def parser(implicit context: Context): Parser[TermDefinition] = {
    for {
      symbol <- Parser.singleWord
      defaultVariables <- Component.variableParser.listInParens(None)
      componentTypes = defaultVariables.map(_.componentType)
      format <- Format.optionalParser(symbol, defaultVariables.map(_.html))
      termSpecification = TermSpecification(symbol, componentTypes, format)
      premises <- premisesParser
      updatedContext = context.copy(termSpecifications = context.termSpecifications :+ termSpecification)
      definitionTemplate <- Statement.parser(updatedContext).inParens
    } yield {
      TermDefinition(termSpecification, premises, defaultVariables, definitionTemplate)
    }
  }

  override def addToContext(termDefinition: TermDefinition, context: Context): Context = {
    context.addTermDefinition(termDefinition)
  }
}

