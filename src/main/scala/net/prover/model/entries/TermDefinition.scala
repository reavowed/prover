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
  override def referencedDefinitions: Set[ChapterEntry] = definingStatement.referencedDefinitions - this ++ premises.flatMap(_.referencedDefinitions).toSet
  override val complexity: Int = definitionPredicate.definitionalComplexity

  override val defaultValue: DefinedTerm = {
    DefinedTerm(componentTypes.map(_.expression), this)(boundVariableNames)
  }
  val definingStatement: Statement = definitionPredicate.specify(Seq(defaultValue), 0, 0).get

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

  override def withSymbol(newSymbol: String): TermDefinition = copy(symbol = newSymbol)
  override def withName(newName: Option[String]): TermDefinition = copy(explicitName = newName)
  override def withShorthand(newShorthand: Option[String]): TermDefinition = copy(shorthand = newShorthand)
  override def withAttributes(newAttributes: Seq[String]): TermDefinition = copy(attributes = newAttributes)
  override def withFormat(newFormat: Format): TermDefinition = copy(format = newFormat)

  override def inferences: Seq[Inference.FromEntry] = Seq(Inference.Definition(name, premises, definingStatement))

  override def serializedLines: Seq[String] = Seq(s"term $symbol $serializedComponents") ++
    (explicitName.map(n => s"name ($n)").toSeq ++
      format.serialized.toSeq ++
      (if (premises.nonEmpty) Seq(s"premises (${premises.map(_.serialized).mkString(", ")})") else Nil) ++
      Seq("(" + definitionPredicate.serialized + ")") ++
      shorthand.map(s => s"shorthand ($s)").toSeq ++
      Some(attributes).filter(_.nonEmpty).map(attributes => s"attributes (${attributes.mkString(" ")})").toSeq
    ).indent

  override def replaceDefinition(
    oldDefinition: ExpressionDefinition,
    newDefinition: ExpressionDefinition,
    entryContext: EntryContext
  ): TermDefinition = {
    TermDefinition(
      symbol,
      boundVariableNames,
      componentTypes,
      explicitName,
      format,
      premises.map(_.replaceDefinition(oldDefinition, newDefinition)),
      definitionPredicate.replaceDefinition(oldDefinition, newDefinition),
      shorthand,
      attributes)
  }

  def apply(components: Expression*): DefinedTerm = {
    DefinedTerm(components, this)(boundVariableNames)
  }
//  def apply(boundVariableNames: String*)(components: Expression*): DefinedTerm = {
//    DefinedTerm(components, this)(boundVariableNames)
//  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[TermDefinition]

  override def equals(other: Any): Boolean = other match {
    case that: TermDefinition =>
      (that canEqual this) &&
        symbol == that.symbol
    case _ => false
  }

  override val hashCode: Int = symbol.hashCode
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
      definitionPredicate <- Statement.parser(expressionParsingContext.addInitialParameters(1)).inParens
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

