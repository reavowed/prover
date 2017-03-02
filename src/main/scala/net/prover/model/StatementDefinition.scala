package net.prover.model

case class StatementDefinition(
    symbol: String,
    componentTypes: Seq[ComponentType],
    format: Format,
    defaultComponents: Seq[Component],
    boundVariables: Seq[TermVariable],
    distinctVariables: DistinctVariables,
    definingStatement: Option[Statement])
  extends ChapterEntry(StatementDefinition)
{
  val defaultStatement = DefinedStatement(defaultComponents, boundVariables, this)

  def statementParser(context: Context): Parser[Statement] = {
    componentTypes.componentsParser(context).map(apply)
  }

  def apply(components: Component*): Statement = {
    DefinedStatement(
      components,
      boundVariables.map { v =>
        components(defaultComponents.indexOf(v))
      }.map(_.asInstanceOf[TermVariable]),
      this)
  }

  def forwardInference: Option[Inference] = definingStatement.map { s =>
    new Inference {
      override val id: String = s"apply-$symbol"
      override val assumption = None
      override val premises: Seq[Statement] = Seq(s)
      override val conclusion: Statement = defaultStatement
      override val arbitraryVariables: Seq[TermVariable] = Nil
      override val distinctVariables: DistinctVariables =
        StatementDefinition.this.distinctVariables
    }
  }

  def reverseInference: Option[Inference] = definingStatement.map { s =>
    new Inference {
      override val id: String = s"unapply-$symbol"
      override val assumption = None
      override val premises: Seq[Statement] = Seq(defaultStatement)
      override val conclusion: Statement = s
      override val arbitraryVariables: Seq[TermVariable] = Nil
      override val distinctVariables: DistinctVariables =
        StatementDefinition.this.distinctVariables
    }
  }
}

object StatementDefinition extends SingleLineChapterEntryParser[StatementDefinition] {
  private def definingStatementParser(context: Context): Parser[Option[Statement]] = Parser.optional(
    "definition",
    Statement.parser(context).inParens.map(Some.apply),
    None)

  private def boundVariablesParser(
    defaultVariables: Seq[Component],
    optionalDefiningStatement: Option[Statement],
    context: Context
  ): Parser[Seq[TermVariable]] = {
    Parser.optional(
      "boundVariables",
      Term.variableListParser(context),
      optionalDefiningStatement.toSeq.flatMap(_.allBoundVariables.intersect(defaultVariables)))
  }

  def distinctVariablesParser(context: Context): Parser[DistinctVariables] = Parser.optional(
    "distinctVariables",
    DistinctVariables.parser(context),
    DistinctVariables.empty)

  def parser(context: Context): Parser[StatementDefinition] = {
    for {
      symbol <- Parser.singleWord
      componentTypes <- ComponentType.listParser
      defaultVariables <- componentTypes.componentsParser(context).inParens
      format <- Format.optionalParser(symbol, componentTypes.length)
      optionalDefiningStatement <- definingStatementParser(context)
      boundVariables <- boundVariablesParser(defaultVariables, optionalDefiningStatement, context)
      distinctVariables <- distinctVariablesParser(context)
    } yield {
      StatementDefinition(
        symbol,
        componentTypes,
        format,
        defaultVariables,
        boundVariables,
        distinctVariables,
        optionalDefiningStatement)
    }
  }

  override def name: String = "statement"
  override def addToContext(t: StatementDefinition, context: Context): Context = {
    context.addStatementDefinition(t)
  }
}
