package net.prover.model

import shapeless.HList

trait StatementParser {
  def symbol: String
  def parseStatement(line: PartialLine, context: Context): (Statement, PartialLine)
}

case class StatementSpecification[Components <: HList](
    symbol: String,
    componentTypeList: ComponentTypeList.Aux[Components],
    format: String,
    requiresBrackets: Boolean)
  extends StatementParser
{

  def apply(components: Components): Statement = DefinedStatement(components, this)

  def unapply(definedStatement: DefinedStatement[_]): Option[Components] = {
    if (definedStatement.statementSpecification == this)
      Some(definedStatement.components.asInstanceOf[Components])
    else
      None
  }

  def formatHtml(components: Components): String = componentTypeList.format(format, components)

  override def parseStatement(line: PartialLine, context: Context): (Statement, PartialLine) = {
    componentTypeList.parse(line, context).mapLeft(apply)
  }
}

case class CustomStatementDefinition[Components <: HList](
    specification: StatementSpecification[Components],
    defaultStatement: Statement,
  definingStatement: Option[Statement])
  extends ChapterEntry(StatementDefinition) with StatementDefinition
{
  override def symbol = specification.symbol
  override def distinctVariables: DistinctVariables = DistinctVariables.empty
  override def parseStatement(line: PartialLine, context: Context): (Statement, PartialLine) = specification.parseStatement(line, context)
}

trait StatementDefinition extends StatementParser {
  def defaultStatement: Statement
  def definingStatement: Option[Statement]
  def distinctVariables: DistinctVariables

  def forwardDeduction: Option[Deduction] = definingStatement.map { s =>
    new Deduction {
      override val id: String = s"apply-$symbol"
      override val premiseTemplates: Seq[Statement] = Seq(s)
      override val conclusionTemplate: Statement = defaultStatement
      override val arbitraryVariables: Seq[TermVariable] = Nil
      override val distinctVariables: DistinctVariables =
        StatementDefinition.this.distinctVariables
    }
  }

  def reverseDeduction: Option[Deduction] = definingStatement.map { s =>
    new Deduction {
      override val id: String = s"unapply-$symbol"
      override val premiseTemplates: Seq[Statement] = Seq(defaultStatement)
      override val conclusionTemplate: Statement = s
      override val arbitraryVariables: Seq[TermVariable] = Nil
      override val distinctVariables: DistinctVariables =
        StatementDefinition.this.distinctVariables
    }
  }
}

object StatementDefinition extends SingleLineChapterEntryParser[CustomStatementDefinition[_]] {
  override def name: String = "statement"
  override def parse(line: PartialLine, context: Context): CustomStatementDefinition[_] = {
    val (symbol, lineAfterSymbol) = line.splitFirstWord
    val (componentTypes, lineAfterComponents) = Parser.inParens(lineAfterSymbol, ComponentTypeList.parse)
    val (format, requiresBrackets, lineAfterFormat) = Parser.parseFormat(lineAfterComponents, symbol, componentTypes.length)
    val statementSpecification = componentTypes.statementSpecification(symbol, format, requiresBrackets)
    val (defaultStatement, lineAfterDefaultStatement) = Parser.inParens(lineAfterFormat, statementSpecification.parseStatement(_, context))
    val (definingStatement, lineAfterDefiningStatement) = Parser.inParens(lineAfterDefaultStatement, Statement.parse(_, context))
    CustomStatementDefinition(statementSpecification, defaultStatement, Some(definingStatement))
  }
  override def addToContext(t: CustomStatementDefinition[_], context: Context): Context = {
    context.addStatementDefinition(t)
  }
}
