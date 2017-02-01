package net.prover.model

case class TermSpecification(
    symbol: String,
    componentTypes: Seq[ComponentType],
    format: Format)
{
  def apply(components: Seq[Component]) = DefinedTerm(components, this)

  def parseTerm(line: PartialLine, context: Context): (Term, PartialLine) = {
    componentTypes.foldLeft((Seq.empty[Component], line)) { case ((components, remainingLine), componentType) =>
      componentType.parse(remainingLine, context).mapLeft(components :+ _)
    }.mapLeft(apply)
  }

  def termParser(context: Context): Parser[Term] = Parser(parseTerm(_, context))
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
    override val premiseTemplates: Seq[Statement] = premises
    override val conclusionTemplate: Statement = definition
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
      defaultComponents <- Components.listParser(componentTypes, context)
      premises <- Statement.listParser(context)
      updatedContext = context.copy(termSpecifications = context.termSpecifications :+ termSpecification)
      definitionTemplate <- Statement.parser(updatedContext).inParens
    } yield {
      TermDefinition(termSpecification, premises, defaultComponents, definitionTemplate)
    }
  }

  override def parse(line: PartialLine, context: Context): TermDefinition = {
    parser(context).parse(line)._1
  }

  override def addToContext(termDefinition: TermDefinition, context: Context): Context = {
    context.addTermDefinition(termDefinition)
  }
}

