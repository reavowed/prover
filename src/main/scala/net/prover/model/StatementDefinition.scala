package net.prover.model

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.Inference.{DirectPremise, Premise}

@JsonIgnoreProperties(Array("symbol", "defaultComponents", "format"))
case class StatementDefinition(
    symbol: String,
    defaultComponents: Seq[Component],
    format: Format,
    boundVariables: Seq[TermVariable],
    distinctVariables: DistinctVariables,
    definingStatement: Option[Statement])
  extends ChapterEntry(StatementDefinition)
{
  val defaultStatement = DefinedStatement(defaultComponents, boundVariables, this)

  private val componentTypes = defaultComponents.map(_.componentType)

  def statementParser(implicit context: Context): Parser[Statement] = {
    componentTypes.componentsParser.map(apply)
  }

  def apply(components: Component*): Statement = {
    DefinedStatement(
      components,
      boundVariables.map { v =>
        components(defaultComponents.indexOf(v))
      }.map(_.asInstanceOf[Term]).map(Term.asVariable),
      this)
  }

  def forwardInference: Option[Inference] = definingStatement.map { s =>
    new Inference {
      override val name = s"Definition of $symbol"
      override val premises: Seq[Premise] = Seq(DirectPremise(s))
      override val conclusion: ProvenStatement = ProvenStatement(defaultStatement, Nil, distinctVariables)
    }
  }

  def reverseInference: Option[Inference] = definingStatement.map { s =>
    new Inference {
      override val name = s"Definition of $symbol"
      override val premises: Seq[Premise] = Seq(DirectPremise(defaultStatement))
      override val conclusion: ProvenStatement = ProvenStatement(s, Nil, distinctVariables)
    }
  }
}

object StatementDefinition extends ChapterEntryParser[StatementDefinition] {
  private def definingStatementParser(implicit context: Context): Parser[Option[Statement]] = Parser.optional(
    "definition",
    Statement.parser.inParens.map(Some.apply),
    None)

  private def boundVariablesParser(
    defaultVariables: Seq[Component],
    optionalDefiningStatement: Option[Statement])(
    implicit context: Context
  ): Parser[Seq[TermVariable]] = {
    Parser.optional(
      "boundVariables",
      Term.variableListParser,
      optionalDefiningStatement.toSeq.flatMap(_.allBoundVariables.intersect(defaultVariables)))
  }

  def distinctVariablesParser(implicit context: Context): Parser[DistinctVariables] = Parser.optional(
    "distinctVariables",
    DistinctVariables.parser,
    DistinctVariables.empty)

  def parser(implicit context: Context): Parser[StatementDefinition] = {
    for {
      symbol <- Parser.singleWord
      defaultVariables <- Component.variableParser.listInParens(None)
      format <- Format.optionalParser(symbol, defaultVariables.map(_.html))
      optionalDefiningStatement <- definingStatementParser
      boundVariables <- boundVariablesParser(defaultVariables, optionalDefiningStatement)
      distinctVariables <- distinctVariablesParser
    } yield {
      StatementDefinition(
        symbol,
        defaultVariables,
        format,
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
