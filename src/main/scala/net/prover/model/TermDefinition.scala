package net.prover.model

import shapeless.HList

case class TermSpecification[Components <: HList](
    symbol: String,
    componentTypes: ComponentTypeList.Aux[Components],
    format: String,
    requiresBrackets: Boolean)
{
  def apply(components: Components): Term = {
    DefinedTerm(components, this)
  }

  def unapply(definedTerm: DefinedTerm[_]): Option[Components] = {
    if (definedTerm.termDefinition == this)
      Some(definedTerm.components.asInstanceOf[Components])
    else
      None
  }

  def parseTerm(line: PartialLine, context: Context): (Term, PartialLine) = {
    componentTypes.parse(line, context).mapLeft(apply)
  }
}

case class TermDefinition[Components <: HList](
    specification: TermSpecification[Components],
    premises: Seq[Statement],
    defaultTerm: Term,
    definition: Statement)
  extends ChapterEntry(TermDefinition)
{
  val id: String = s"definition-${specification.symbol}"
  val deduction: Deduction = new Deduction {
    override val id: String = TermDefinition.this.id
    override val premiseTemplates: Seq[Statement] = premises
    override val conclusionTemplate: Statement = definition
    override val arbitraryVariables: Seq[TermVariable] = Nil
    override val distinctVariables: DistinctVariables = DistinctVariables.empty
  }
}

object TermDefinition extends SingleLineChapterEntryParser[TermDefinition[_]] {
  override val name: String = "term"

  override def parse(line: PartialLine, context: Context): TermDefinition[_] = {
    val (symbol, lineAfterSymbol) = line.splitFirstWord
    val (componentTypes, lineAfterComponents) = Parser.inParens(lineAfterSymbol, ComponentTypeList.parse)
    val (format, requiresBrackets, lineAfterFormat) = Parser.parseFormat(lineAfterComponents, symbol, componentTypes.length)
    val termSpecification = componentTypes.termSpecification(symbol, format, requiresBrackets)
    val (defaultTerm, lineAfterDefaultTerm) = Parser.inParens(lineAfterFormat, termSpecification.parseTerm(_, context))
    val (premises, lineAfterPremises) = Parser.listInParens(lineAfterDefaultTerm, Statement.parse(_, context))
    val updatedContext = context.copy(termSpecifications = context.termSpecifications :+ termSpecification)
    val (definitionTemplate, _) = Parser.inParens(lineAfterPremises, Statement.parse(_, updatedContext))
    TermDefinition(termSpecification, premises, defaultTerm, definitionTemplate)
  }
  override def addToContext(termDefinition: TermDefinition[_], context: Context): Context = {
    context.copy(
      termSpecifications = context.termSpecifications :+ termDefinition.specification,
      theoremLineParsers = context.theoremLineParsers :+ termDefinition.deduction)
  }
}

