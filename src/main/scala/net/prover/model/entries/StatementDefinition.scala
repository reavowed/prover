package net.prover.model.entries

import net.prover.model._
import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.expressions._

case class StatementDefinition(
    baseSymbol: String,
    boundVariableNames: Seq[String],
    componentTypes: Seq[ComponentType],
    explicitName: Option[String],
    format: Format,
    definingStatement: Option[Statement],
    shorthand: Option[String],
    attributes: Seq[String])
  extends ExpressionDefinition with TypedExpressionDefinition[StatementDefinition]
{
  override def disambiguatedSymbol: DisambiguatedSymbol = DisambiguatedSymbol(baseSymbol, None)
  override def name: String = explicitName.getOrElse(disambiguatedSymbol.forDisplay)
  override def typeName: String = "Statement"
  override def referencedDefinitions: Set[ChapterEntry] = definingStatement.map(_.referencedDefinitions).getOrElse(Set.empty).toType[ChapterEntry] - this
  override val complexity: Int = definingStatement.map(_.definitionalComplexity).getOrElse(1)

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
      DefinedStatementTemplate(this, newBoundVariableNames, components)
    }
  }

  override def withSymbol(newSymbol: String): StatementDefinition = copy(baseSymbol = newSymbol)
  override def withName(newName: Option[String]): StatementDefinition = copy(explicitName = newName)
  override def withShorthand(newShorthand: Option[String]): StatementDefinition = copy(shorthand = newShorthand)
  override def withAttributes(newAttributes: Seq[String]): StatementDefinition = copy(attributes = newAttributes)
  override def withFormat(newFormat: Format): StatementDefinition = copy(format = newFormat)

  val constructionInference = definingStatement.map(s => Inference.Definition(name, Seq(s), defaultValue))
  val deconstructionInference = definingStatement.map(s => Inference.Definition(name, Seq(defaultValue), s))

  override def inferences: Seq[Inference.FromEntry] = constructionInference.toSeq ++ deconstructionInference.toSeq

  override def serializedLines: Seq[String] = Seq(s"statement $baseSymbol $serializedComponents") ++
    (explicitName.map(n => s"name ($n)").toSeq ++
      format.serialized.toSeq ++
      definingStatement.map(s => s"definition (${s.serialized})").toSeq ++
      shorthand.map(s => s"shorthand ($s)").toSeq ++
      Some(attributes).filter(_.nonEmpty).map(attributes => s"attributes (${attributes.mkString(" ")})").toSeq
    ).indent

  override def replaceDefinition(
    oldDefinition: ExpressionDefinition,
    newDefinition: ExpressionDefinition,
    entryContext: EntryContext
  ): StatementDefinition = {
    StatementDefinition(
      baseSymbol,
      boundVariableNames,
      componentTypes,
      explicitName,
      format,
      definingStatement.map(_.replaceDefinition(oldDefinition, newDefinition)),
      shorthand,
      attributes)
  }

  def apply(components: Expression*): DefinedStatement = {
    DefinedStatement(components, this)(boundVariableNames)
  }
  def bind(boundVariableNames: String*)(components: Expression*): DefinedStatement = {
    DefinedStatement(components, this)(boundVariableNames)
  }
  def unapplySeq(expression: Expression): Option[Seq[Expression]] = expression match {
    case DefinedStatement(components, definition) if definition == this =>
      Some(components)
    case _ =>
      None
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[StatementDefinition]

  override def equals(other: Any): Boolean = other match {
    case that: StatementDefinition =>
      (that canEqual this) &&
        symbol == that.symbol
    case _ => false
  }

  override val hashCode: Int = symbol.hashCode
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
      format <- Format.optionalParserForExpressionDefinition(symbol, boundVariables, componentTypes)
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
