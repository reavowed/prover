package net.prover.model

case class TermSpecification(
    symbol: String,
    componentTypes: Seq[ComponentType],
    format: Format)
{
  def apply(components: Seq[Component]) = DefinedTerm(components, this)

  def termParser(context: Context): Parser[Term] = {
    componentTypes.componentsParser(context).map(apply)
  }
}

case class TermDefinition(
    specification: TermSpecification,
    premises: Seq[Statement],
    defaultComponents: Seq[Component],
    definition: Statement)
  extends ChapterEntry(TermDefinition)
{
  val id: String = s"definition-${specification.symbol}"
  val defaultTerm = DefinedTerm(defaultComponents, specification)
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

  def parser(context: Context): Parser[TermDefinition] = {
    for {
      symbol <- Parser.singleWord
      componentTypes <- ComponentType.listParser
      format <- Format.parser(symbol, componentTypes.length)
      termSpecification = TermSpecification(symbol, componentTypes, format)
      defaultComponents <- componentTypes.componentsParser(context).inParens
      premises <- Statement.listParser(context)
      updatedContext = context.copy(termSpecifications = context.termSpecifications :+ termSpecification)
      definitionTemplate <- Statement.parser(updatedContext).inParens
    } yield {
      TermDefinition(termSpecification, premises, defaultComponents, definitionTemplate)
    }
  }

  override def addToContext(termDefinition: TermDefinition, context: Context): Context = {
    context.addTermDefinition(termDefinition)
  }
}

