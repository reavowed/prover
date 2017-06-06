package net.prover.model

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.Inference.{DirectPremise, RearrangementType}

@JsonIgnoreProperties(Array("symbol", "defaultComponents", "format"))
case class StatementDefinition(
    symbol: String,
    defaultComponents: Seq[Component],
    name: String,
    format: Format,
    definingStatement: Option[Statement],
    boundVariables: Set[TermVariable],
    distinctVariables: DistinctVariables)
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
      None,
      s"Definition of $name",
      Seq(DirectPremise(s)),
      ProvenStatement(defaultStatement, Conditions(Set.empty, distinctVariables)),
      RearrangementType.NotRearrangement,
      allowsRearrangement = true)
  }

  def reverseInference: Option[Inference] = definingStatement.map { s =>
    DerivedInference(
      None,
      s"Definition of $name",
      Seq(DirectPremise(defaultStatement)),
      ProvenStatement(s, Conditions(Set.empty , distinctVariables)),
      RearrangementType.NotRearrangement,
      allowsRearrangement = true)
  }
}

object StatementDefinition extends ChapterEntryParser[StatementDefinition] {
  def nameParser(implicit context: Context): Parser[Option[String]] = Parser.optional(
    "name",
    Parser.allInParens)

  private def definingStatementParser(implicit context: Context): Parser[Option[Statement]] = Parser.optional(
    "definition",
    Statement.parser.inParens)

  private def boundVariablesParser(
    defaultVariables: Seq[Variable],
    optionalDefiningStatement: Option[Statement])(
    implicit context: Context
  ): Parser[Set[TermVariable]] = {
    optionalDefiningStatement match {
      case Some(definingStatement) =>
        val variables = Variables(
          defaultVariables.ofType[StatementVariable].toSet,
          defaultVariables.ofType[TermVariable].toSet)
        val boundVariables = variables.termVariables.filter { defaultVariable =>
          definingStatement.getPotentiallyIntersectingVariables(defaultVariable).intersect(variables).isEmpty
        }
        Parser.constant(boundVariables)
      case None =>
        Parser.optional(
          "boundVariables",
          Term.variableListParser.map(_.toSet),
          Set.empty)
    }
  }

  def parser(implicit context: Context): Parser[StatementDefinition] = {
    for {
      symbol <- Parser.singleWord
      defaultVariables <- Component.variableParser.listInParens(None)
      name <- nameParser.getOrElse(symbol)
      format <- Format.optionalParser(symbol, defaultVariables.map(_.html))
      optionalDefiningStatement <- definingStatementParser
      boundVariables <- boundVariablesParser(defaultVariables, optionalDefiningStatement)
      distinctVariables <- Conditions.distinctVariablesParser
    } yield {
      StatementDefinition(
        symbol,
        defaultVariables,
        name,
        format,
        optionalDefiningStatement,
        boundVariables,
        distinctVariables)
    }
  }

  override def name: String = "statement"
  override def addToContext(t: StatementDefinition, context: Context): Context = {
    context.addStatementDefinition(t)
  }
}
