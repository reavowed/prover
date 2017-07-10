package net.prover.model.entries

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.model.components._
import net.prover.model.{Conditions, DefinitionInference, DistinctVariables, Format, Inference, Parser, ParsingContext}

@JsonIgnoreProperties(Array("symbol", "defaultVariables", "format"))
case class StatementDefinition(
    symbol: String,
    defaultVariables: Seq[Component],
    name: String,
    format: Format,
    definingStatement: Option[Statement],
    boundVariables: Set[TermVariable],
    distinctVariables: DistinctVariables)
  extends ChapterEntry(StatementDefinition)
{
  val defaultValue = DefinedStatement(defaultVariables, boundVariables, this)

  private val componentTypes = defaultVariables.map(_.componentType)

  def statementParser(implicit context: ParsingContext): Parser[Statement] = {
    componentTypes.componentsParser.map(apply)
  }

  def apply(components: Component*): Statement = {
    DefinedStatement(
      components,
      boundVariables.map { v =>
        components(defaultVariables.indexOf(v))
      }.map(_.asInstanceOf[Term]).map(Term.asVariable),
      this)
  }

  override def inferences: Seq[Inference] = {
    definingStatement.toSeq.flatMap { s =>
      Seq(
        DefinitionInference(name, Seq(s), defaultValue, distinctVariables),
        DefinitionInference(name, Seq(defaultValue), s, distinctVariables))
    }
  }
}

object StatementDefinition extends ChapterEntryParser[StatementDefinition] {
  override val name: String = "statement"

  def nameParser(implicit context: ParsingContext): Parser[Option[String]] = Parser.optional(
    "name",
    Parser.allInParens)

  private def definingStatementParser(implicit context: ParsingContext): Parser[Option[Statement]] = Parser.optional(
    "definition",
    Statement.parser.inParens)

  private def boundVariablesParser(
    defaultVariables: Seq[Variable],
    optionalDefiningStatement: Option[Statement])(
    implicit context: ParsingContext
  ): Parser[Set[TermVariable]] = {
    optionalDefiningStatement match {
      case Some(definingStatement) =>
        val boundVariables = defaultVariables.ofType[TermVariable].toSet.filter { v =>
          definingStatement.boundVariables.contains(v) || !definingStatement.presentVariables.contains(v)
        }
        Parser.constant(boundVariables)
      case None =>
        Parser.optional(
          "boundVariables",
          Term.variableListParser.map(_.toSet),
          Set.empty)
    }
  }

  def parser(implicit context: ParsingContext): Parser[StatementDefinition] = {
    for {
      symbol <- Parser.singleWord
      defaultVariables <- Variable.parser.listInParens(None)
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
}
