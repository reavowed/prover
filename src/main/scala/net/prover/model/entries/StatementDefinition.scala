package net.prover.model.entries

import net.prover.model._
import net.prover.model.entries.ExpressionDefinition.{ComponentType, TermComponent}
import net.prover.model.expressions._

case class StatementDefinition(
    symbol: String,
    boundVariableNames: Seq[String],
    componentTypes: Seq[ComponentType],
    explicitName: Option[String],
    format: Format,
    definingStatement: Option[Statement],
    shorthand: Option[String],
    attributes: Seq[String])
  extends ExpressionDefinition with TypedExpressionDefinition[StatementDefinition]
{
  override def name: String = explicitName.getOrElse(symbol)
  override def typeName: String = "Statement"
  override def referencedDefinitions: Set[ExpressionDefinition] = definingStatement.map(_.referencedDefinitions).getOrElse(Set.empty)

  val defaultValue: DefinedStatement = {
    DefinedStatement(componentTypes.map(_.expression), this)(boundVariableNames)
  }

  def statementParser(implicit context: ExpressionParsingContext): Parser[Statement] = {
    componentExpressionParser.map { case (newBoundVariableNames, components) =>
      DefinedStatement(components, this)(newBoundVariableNames)
    }
  }

  def templateParser(implicit templateParsingContext: TemplateParsingContext): Parser[Template] = {
    componentTemplateParser.map { case (newBoundVariableNames, components) =>
      Template.DefinedStatement(this, newBoundVariableNames, components)
    }
  }

  override def withShorthand(newShorthand: Option[String]): StatementDefinition = copy(shorthand = newShorthand)

  override def inferences: Seq[Inference] = {
    definingStatement.toSeq.flatMap { s =>
      Seq(
        Inference.Definition(name, Seq(s), defaultValue),
        Inference.Definition(name, Seq(defaultValue), s))
    }
  }

  override def serializedLines: Seq[String] = Seq(s"statement $symbol $serializedComponents") ++
    (explicitName.map(n => s"name ($n)").toSeq ++
      format.serialized.map(f => s"format ($f)").toSeq ++
      definingStatement.map(s => s"definition (${s.serialized})").toSeq ++
      shorthand.map(s => s"shorthand ($s)").toSeq ++
      Some(attributes).filter(_.nonEmpty).map(attributes => s"attributes (${attributes.mkString(" ")})").toSeq
    ).indent
}

object StatementDefinition extends ChapterEntryParser {
  override val name: String = "statement"

  def nameParser: Parser[Option[String]] = Parser.optional(
    "name",
    Parser.allInParens)

  private def definingStatementParser(implicit context: ExpressionParsingContext): Parser[Option[Statement]] = Parser.optional(
    "definition",
    Statement.parser.inParens)

  def parser(implicit entryContext: EntryContext): Parser[StatementDefinition] = {
    for {
      symbol <- Parser.singleWord
      boundVariablesAndComponentTypes <- ExpressionDefinition.boundVariablesAndComponentTypesParser
      boundVariables = boundVariablesAndComponentTypes._1
      componentTypes = boundVariablesAndComponentTypes._2
      name <- nameParser
      format <- Format.optionalParser(symbol, boundVariables ++ componentTypes.map(_.name))
      optionalDefiningStatement <- definingStatementParser(ExpressionParsingContext.outsideProof(entryContext))
      shorthand <- ExpressionDefinition.shorthandParser
      attributes <- ExpressionDefinition.attributesParser
    } yield {
      StatementDefinition(
        symbol,
        boundVariables,
        componentTypes,
        name,
        format,
        optionalDefiningStatement,
        shorthand,
        attributes)
    }
  }
}
