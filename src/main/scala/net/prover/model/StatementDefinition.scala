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

  def parseStatement(line: PartialLine, context: Context): (Statement, PartialLine) = {
    componentTypes.foldLeft((Seq.empty[Component], line)) { case ((components, remainingLine), componentType) =>
      componentType.parse(remainingLine, context).mapLeft(components :+ _)
    }.mapLeft(apply)
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
  def parser(context: Context): Parser[StatementDefinition] = {
    for {
      symbol <- Parser.singleWord
      componentTypes <- ComponentType.listParser
      format <- Format.parser(symbol, componentTypes.length)
      defaultVariables <- Components.listParser(componentTypes, context)
      optionalDefiningStatement <- Statement.parser(context).optionalInParens
      boundVariables <- optionalDefiningStatement match {
        case Some(x) =>
          Parser.constant(x.allBoundVariables.intersect(defaultVariables))
        case None =>
          Term.variableListParser(context)
      }
      distinctVariables <- DistinctVariables.parser(context)
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

  override def parse(line: PartialLine, context: Context): StatementDefinition = {
    parser(context).parse(line)._1
  }

  override def name: String = "statement"
  override def addToContext(t: StatementDefinition, context: Context): Context = {
    context.addStatementDefinition(t)
  }
}
