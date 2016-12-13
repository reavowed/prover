package net.prover.model

import shapeless.HList

case class TermDefinition[Components <: HList](
    symbol: String,
    componentTypes: ComponentTypeList.Aux[Components],
    format: String,
    definition: Option[Statement])
  extends ChapterEntry(TermDefinition) with TermParser
{
  val id: String = s"definition-$symbol"
  val defaultTerm: Term = apply(componentTypes.defaults())

  override def parseTerm(line: PartialLine, context: Context): (Term, PartialLine) = {
    componentTypes.parse(line, context).mapLeft(apply)
  }

  def apply(components: Components): Term = {
    DefinedTerm(components, this)
  }

  def unapply(definedTerm: DefinedTerm[_]): Option[Components] = {
    if (definedTerm.termDefinition == this)
      Some(definedTerm.components.asInstanceOf[Components])
    else
      None
  }

  val deduction: Option[Deduction] = definition.map { d =>
    new Deduction {
      override val id: String = s"definition-$symbol"
      override val premiseTemplates: Seq[Statement] = Nil
      override val conclusionTemplate: Statement = d
      override val arbitraryVariables: Seq[TermVariable] = Nil
      override val distinctVariableRequirements: DistinctVariableRequirements = DistinctVariableRequirements.empty
    }
  }
}

object TermDefinition extends SingleLineChapterEntryParser[TermDefinition[_]] {
  override val name: String = "term"

  private def parseComponentTypeList(line: PartialLine): (ComponentTypeList, PartialLine) = {
    line match {
      case WordAndRemainingText("term", lineAfterTerm) =>
        val (innerComponentTypeList, remainingLine) = parseComponentTypeList(lineAfterTerm)
        (ComponentTypeList.withTerm(innerComponentTypeList), remainingLine)
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
    val format = formatOption match {
      case Some(f) =>
        f
      case None if componentTypes.length == 2 =>
        s"{} $symbol {}"
      case None if componentTypes.length == 1 =>
        s"$symbol {}"
      case None if componentTypes.length == 0 =>
        symbol
      case _ =>
        throw ParseException.withMessage("Explicit format must be supplied with more than two componenets", line.fullLine)
    }
    val (definitionTemplate, _) = Statement.parse(
      lineAfterFormat,
      addToContext(componentTypes.termDefinition(symbol, format, None), context))
    componentTypes.termDefinition(symbol, format, Some(definitionTemplate))
  }
  override def addToContext(termDefinition: TermDefinition[_], context: Context): Context = {
    context.copy(termDefinitions = context.termDefinitions :+ termDefinition)
  }
}

