package net.prover.model

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.Inference.{DirectPremise, Premise}

@JsonIgnoreProperties(Array("symbol", "defaultComponents", "format"))
case class StatementDefinition(
    symbol: String,
    defaultComponents: Seq[Component],
    format: Format,
    boundVariables: Set[TermVariable],
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
    DerivedInference(
      s"Definition of $symbol",
      Seq(DirectPremise(s)),
      ProvenStatement.withNoConditions(defaultStatement))
  }

  def reverseInference: Option[Inference] = definingStatement.map { s =>
    DerivedInference(
      s"Definition of $symbol",
      Seq(DirectPremise(defaultStatement)),
      ProvenStatement.withNoConditions(s))
  }
}

object StatementDefinition extends ChapterEntryParser[StatementDefinition] {
  private def getDefaultBoundVariables(
    defaultVariables: Seq[Component],
    definingStatement: Statement
  ): Set[TermVariable] = {
    val variables = Variables(
      defaultVariables.ofType[StatementVariable].toSet,
      defaultVariables.ofType[TermVariable].toSet)
    variables.termVariables.filter { defaultVariable =>
      definingStatement.getPotentiallyIntersectingVariables(defaultVariable).intersect(variables).isEmpty
    }
  }

  private def definingStatementParser(implicit context: Context): Parser[Option[Statement]] = Parser.optional(
    "definition",
    Statement.parser.inParens.map(Some.apply),
    None)

  private def boundVariablesParser(
    defaultVariables: Seq[Component],
    optionalDefiningStatement: Option[Statement])(
    implicit context: Context
  ): Parser[Set[TermVariable]] = {
    Parser.optional(
      "boundVariables",
      Term.variableListParser.map(_.toSet),
      optionalDefiningStatement.map(getDefaultBoundVariables(defaultVariables, _)).getOrElse(Set.empty))
  }

  def parser(implicit context: Context): Parser[StatementDefinition] = {
    for {
      symbol <- Parser.singleWord
      defaultVariables <- Component.variableParser.listInParens(None)
      format <- Format.optionalParser(symbol, defaultVariables.map(_.html))
      optionalDefiningStatement <- definingStatementParser
      boundVariables <- boundVariablesParser(defaultVariables, optionalDefiningStatement)
    } yield {
      StatementDefinition(
        symbol,
        defaultVariables,
        format,
        boundVariables,
        optionalDefiningStatement)
    }
  }

  override def name: String = "statement"
  override def addToContext(t: StatementDefinition, context: Context): Context = {
    context.addStatementDefinition(t)
  }
}
