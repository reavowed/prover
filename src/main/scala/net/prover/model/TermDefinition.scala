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
    definition: Statement)
  extends ChapterEntry(TermDefinition)
{
  val id: String = s"definition-${specification.symbol}"
  val defaultTerm: Term = specification(specification.componentTypes.defaults())

  val deduction: Deduction = new Deduction {
    override val id: String = TermDefinition.this.id
    override val premiseTemplates: Seq[Statement] = Nil
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

  override def parse(line: PartialLine, context: Context): TermDefinition[_] = {
    val (symbol, lineAfterSymbol) = line.splitFirstWord
    val (componentTypes, lineAfterComponents) = parseComponentTypeList(lineAfterSymbol)
    val (formatOption, lineAfterFormat) = lineAfterComponents match {
      case WordAndRemainingText(f, remainingLine) if f.contains('{') =>
        (Some(f), remainingLine)
      case _ =>
        (None, lineAfterComponents)
    }
    val (format, requiresBrackets, lineAfterBrackets) = formatOption match {
      case Some(f) =>
        val (requiresBrackets, lineAfterBrackets) = lineAfterFormat match {
          case WordAndRemainingText("requiresBrackets", remainingLine) =>
            (true, remainingLine)
          case _ =>
            (false, lineAfterFormat)
        }
        (f, requiresBrackets, lineAfterBrackets)
      case None if componentTypes.length == 2 =>
        (s"{} $symbol {}", true, lineAfterFormat)
      case None if componentTypes.length == 1 =>
        (s"$symbol {}", false, lineAfterFormat)
      case None if componentTypes.length == 0 =>
        (symbol, false, lineAfterFormat)
      case _ =>
        throw ParseException.withMessage("Explicit format must be supplied with more than two componenets", line.fullLine)
    }
    val termSpecification = componentTypes.termSpecification(symbol, format, requiresBrackets)
    val (definitionTemplate, _) = Statement.parse(
      lineAfterBrackets,
      context.copy(termParsers = context.termParsers :+ termSpecification))
    TermDefinition(termSpecification, definitionTemplate)
  }
  override def addToContext(termDefinition: TermDefinition[_], context: Context): Context = {
    context.copy(
      termParsers = context.termParsers :+ termDefinition.specification,
      otherTheoremLineParsers = context.otherTheoremLineParsers :+ termDefinition.deduction)
  }
}

