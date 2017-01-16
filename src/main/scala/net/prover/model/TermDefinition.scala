package net.prover.model

import shapeless.HList

case class TermSpecification[Components <: HList](
    symbol: String,
    componentTypes: ComponentTypeList.Aux[Components],
    format: String,
    requiresBrackets: Boolean)
  extends TermParser
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

  override def parseTerm(line: PartialLine, context: Context): (Term, PartialLine) = {
    componentTypes.parse(line, context).mapLeft(apply)
  }
}

case class TermDefinition[Components <: HList](
    specification: TermSpecification[Components],
    premises: Seq[Statement],
    definition: Statement)
  extends ChapterEntry(TermDefinition)
{
  val id: String = s"definition-${specification.symbol}"
  val defaultTerm: Term = specification(specification.componentTypes.defaults())

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

  private def parseComponentTypeList(line: PartialLine): (ComponentTypeList, PartialLine) = {
    line match {
      case WordAndRemainingText("term", lineAfterTerm) =>
        val (innerComponentTypeList, remainingLine) = parseComponentTypeList(lineAfterTerm)
        (ComponentTypeList.withTerm(innerComponentTypeList), remainingLine)
      case WordAndRemainingText("statement", lineAfterTerm) =>
        val (innerComponentTypeList, remainingLine) = parseComponentTypeList(lineAfterTerm)
        (ComponentTypeList.withStatement(innerComponentTypeList), remainingLine)
      case _ =>
        (ComponentTypeList.empty, line)
    }
  }

  private def parseRawFormat(line: PartialLine): (String, PartialLine) = {
    line.toEndOfParens
  }

  private def readFormat(rawFormat: String, symbol: String, numberOfComponents: Int)(implicit line: BookLine): (String, Boolean) = rawFormat match {
    case f if f.nonEmpty =>
      (f, false)
    case "" if numberOfComponents == 2 =>
      (s"{} $symbol {}", true)
    case "" if numberOfComponents == 1 =>
      (s"$symbol {}", false)
    case "" if numberOfComponents == 0 =>
      (symbol, false)
    case "" =>
      throw ParseException.withMessage("Explicit format must be supplied with more than two components", line)
  }

  override def parse(line: PartialLine, context: Context): TermDefinition[_] = {
    val (symbol, lineAfterSymbol) = line.splitFirstWord
    val (componentTypes, lineAfterComponents) = Parser.inParens(lineAfterSymbol, parseComponentTypeList)
    val (rawFormat, lineAfterFormat) = Parser.inParens(lineAfterComponents, parseRawFormat)
    val (format, requiresBrackets) = readFormat(rawFormat, symbol, componentTypes.length)(line.fullLine)
    val termSpecification = componentTypes.termSpecification(symbol, format, requiresBrackets)
    val (premises, lineAfterPremises) = Parser.listInParens(lineAfterFormat, Statement.parse(_, context))
    val updatedContext = context.copy(termParsers = context.termParsers :+ termSpecification)
    val (definitionTemplate, _) = Parser.inParens(lineAfterPremises, Statement.parse(_, updatedContext))
    TermDefinition(termSpecification, premises, definitionTemplate)
  }
  override def addToContext(termDefinition: TermDefinition[_], context: Context): Context = {
    context.copy(
      termParsers = context.termParsers :+ termDefinition.specification,
      otherTheoremLineParsers = context.otherTheoremLineParsers :+ termDefinition.deduction)
  }
}

