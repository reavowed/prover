package net.prover.model

import shapeless.{::, HList, HNil}

trait ComponentTypeList{
  type Components <: HList
  def length: Int
  def parse(line: PartialLine, context: Context): (Components, PartialLine)
  def defaults(currentStatement: Int = 1, currentTerm: Int = 1): Components
  def format(formatString: String, components: Components): String
}

object ComponentTypeList {
  type Aux[T <: HList] = ComponentTypeList {type Components = T}

  val empty: ComponentTypeList = new ComponentTypeList {
    type Components = HNil
    val length: Int = 0
    override def parse(line: PartialLine, context: Context): (HNil, PartialLine) = (HNil, line)
    override def defaults(currentStatement: Int, currentTerm: Int) = HNil
    override def format(formatString: String, components: HNil): String = formatString
  }

  def withTerm(inner: ComponentTypeList) = new ComponentTypeList {
    type Components = Term :: inner.Components
    val length: Int = inner.length + 1
    override def parse(line: PartialLine, context: Context): (Term :: inner.Components, PartialLine) = {
      val (term, lineAfterTerm) = Term.parse(line, context)
      val (otherComponents, remainingLine) = inner.parse(lineAfterTerm, context)
      (::(term, otherComponents), remainingLine)
    }

    override def defaults(currentStatement: Int, currentTerm: Int): Term :: inner.Components = {
      val innerDefaults = inner.defaults(currentStatement, currentTerm + 1)
      ::(TermVariable(currentTerm), innerDefaults)
    }

    override def format(
      formatString: String,
      components: Term :: inner.Components
    ): String = {
      val updatedFormatString = formatString.replaceFirst("\\{\\}", components.head.toString)
      inner.format(updatedFormatString, components.tail)
    }
  }
}

case class TermDefinition(
    symbol: String,
    componentTypes: ComponentTypeList,
    format: String,
    definition: Option[Statement])
  extends ChapterEntry(TermDefinition) with TermParser
{
  val id: String = s"definition-$symbol"
  val defaultTerm: Term = apply(componentTypes.defaults())

  override def parseTerm(line: PartialLine, context: Context): (Term, PartialLine) = {
    componentTypes.parse(line, context).mapLeft(apply)
  }

  def apply(components: componentTypes.Components): Term = {
    DefinedTerm[componentTypes.Components](symbol, format, components, componentTypes)
  }

  def definitionStepParser: Option[DirectStepParser] = definition.map{ statement => new DirectStepParser {
    override val id: String = s"definition-$symbol"
    override def readStep(theoremBuilder: TheoremBuilder, line: PartialLine, context: Context) = {
      matchPremisesToConclusion(Nil, statement, line, context).mapLeft(Step(_))
    }
  }}
}

object TermDefinition extends SingleLineChapterEntryParser[TermDefinition] {
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

  override def parse(line: PartialLine, context: Context): TermDefinition = {
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
      addToContext(TermDefinition(symbol, componentTypes, format, None), context))
    TermDefinition(symbol, componentTypes, format, Some(definitionTemplate))
  }
  override def addToContext(termDefinition: TermDefinition, context: Context): Context = {
    context.copy(termDefinitions = context.termDefinitions :+ termDefinition)
  }
}

