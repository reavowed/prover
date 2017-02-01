package net.prover.model

trait StatementParser {
  def symbol: String
  def parseStatement(line: PartialLine, context: Context): (Statement, PartialLine)
}

case class StatementSpecification(
    symbol: String,
    componentTypes: Seq[ComponentType],
    format: Format)
  extends StatementParser
{
  override def parseStatement(line: PartialLine, context: Context): (Statement, PartialLine) = {
    componentTypes.foldLeft((Seq.empty[Component], line)) { case ((components, remainingLine), componentType) =>
      componentType.parse(remainingLine, context).mapLeft(components :+ _)
    }.mapLeft(DefinedStatement(_, this))
  }
}

case class CustomStatementDefinition(
    specification: StatementSpecification,
    defaultComponents: Seq[Component],
    definingStatement: Option[Statement])
  extends ChapterEntry(StatementDefinition) with StatementDefinition
{
  override val defaultStatement = DefinedStatement(defaultComponents, specification)
  override def symbol = specification.symbol
  override def distinctVariables: DistinctVariables = DistinctVariables.empty
  override def parseStatement(line: PartialLine, context: Context): (Statement, PartialLine) = specification.parseStatement(line, context)

  def apply(components: Component*): Statement = {
    DefinedStatement(components, specification)
  }
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

object StatementDefinition extends SingleLineChapterEntryParser[CustomStatementDefinition] {
  private def definingStatementParser(context: Context): Parser[Option[Statement]] = {
    Parser(Statement.parse(_, context)).optionalInParens
  }

  def parser(context: Context): Parser[CustomStatementDefinition] = {
    for {
      symbol <- Parser.singleWord
      componentTypes <- ComponentType.listParser
      format <- Format.parser(symbol, componentTypes.length)
      defaultVariables <- Components.listParser(componentTypes, context)
      optionalDefiningStatement <- definingStatementParser(context)
      statementSpecification = StatementSpecification(symbol, componentTypes, format)
    } yield {
      CustomStatementDefinition(statementSpecification, defaultVariables, optionalDefiningStatement)
    }
  }

  override def parse(line: PartialLine, context: Context): CustomStatementDefinition = {
    parser(context).parse(line)._1
  }

  override def name: String = "statement"
  override def addToContext(t: CustomStatementDefinition, context: Context): Context = {
    context.addStatementDefinition(t)
  }
}
