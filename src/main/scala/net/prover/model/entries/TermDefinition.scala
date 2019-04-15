package net.prover.model.entries

import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.expressions._
import net.prover.model._

case class TermDefinition(
    symbol: String,
    boundVariableNames: Seq[String],
    componentTypes: Seq[ComponentType],
    explicitName: Option[String],
    format: Format,
    premises: Seq[Statement],
    definitionPredicate: Statement,
    shorthand: Option[String],
    attributes: Seq[String])
  extends ExpressionDefinition with TypedExpressionDefinition[TermDefinition]
{
  override def name: String = explicitName.getOrElse(symbol)
  override def typeName: String = "Term"
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions ++ premises.flatMap(_.referencedDefinitions).toSet

  override val defaultValue: DefinedTerm = {
    DefinedTerm(componentTypes.map(_.expression), this)(boundVariableNames)
  }
  val definingStatement: Statement = definitionPredicate.specify(Seq(defaultValue), 0, 0)

  def termParser(implicit context: ExpressionParsingContext): Parser[Term] = {
    componentExpressionParser.map { case (newBoundVariableNames, components) =>
      DefinedTerm(components, this)(newBoundVariableNames)
    }
  }

  def templateParser(implicit context: TemplateParsingContext): Parser[Template] = {
    componentTemplateParser.map { case (newBoundVariableNames, components) =>
      Template.DefinedTerm(this, newBoundVariableNames, components)
    }
  }

  override def withShorthand(newShorthand: Option[String]): TermDefinition = copy(shorthand = newShorthand)

  override def inferences: Seq[Inference] = Seq(Inference.Definition(name, premises, definingStatement))

  override def serializedLines: Seq[String] = Seq(s"term $symbol $serializedComponents") ++
    (explicitName.map(n => s"name ($n)").toSeq ++
      format.serialized.map(f => s"format ($f)").toSeq ++
      (if (premises.nonEmpty) Seq(s"premises (${premises.map(_.serialized).mkString(", ")})") else Nil) ++
      Seq("(" + definitionPredicate.serialized + ")") ++
      shorthand.map(s => s"shorthand ($s)").toSeq ++
      Some(attributes).filter(_.nonEmpty).map(attributes => s"attributes (${attributes.mkString(" ")})").toSeq
    ).indent
}

object TermDefinition extends ChapterEntryParser {
  override val name: String = "term"

  def premisesParser(implicit context: ExpressionParsingContext): Parser[Seq[Statement]] = Parser.optional(
    "premises",
    Statement.listParser,
    Nil)

  def nameParser: Parser[Option[String]] = Parser.optional(
    "name",
    Parser.allInParens)

  def parser(implicit entryContext: EntryContext): Parser[TermDefinition] = {
    implicit val expressionParsingContext: ExpressionParsingContext = ExpressionParsingContext.outsideProof(entryContext)
    for {
      symbol <- Parser.singleWord
      boundVariablesAndComponentTypes <- ExpressionDefinition.boundVariablesAndComponentTypesParser
      boundVariables = boundVariablesAndComponentTypes._1
      componentTypes = boundVariablesAndComponentTypes._2
      name <- nameParser
      format <- Format.optionalParser(symbol, boundVariables ++ componentTypes.map(_.name))
      premises <- premisesParser
      definitionPredicate <- Statement.parser(expressionParsingContext.addParameters("_")).inParens
      shorthand <- ExpressionDefinition.shorthandParser
      attributes <- ExpressionDefinition.attributesParser
    } yield {
      TermDefinition(
        symbol,
        boundVariables,
        componentTypes,
        name,
        format,
        premises,
        definitionPredicate,
        shorthand,
        attributes)
    }
  }
}

