package net.prover.model.entries

import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model._
import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.entries.ExpressionDefinition.ComponentType.TermComponent
import net.prover.model.expressions.Statement

case class TypeDefinition(
    symbol: String,
    defaultTermName: String,
    otherComponentTypes: Seq[ComponentType],
    componentFormat: Format.Explicit,
    explicitName: Option[String],
    definingStatement: Statement)
  extends ChapterEntry.Standalone
{
  override val name: String = explicitName.getOrElse(symbol)
  override val title: String = s"Type Definition: ${name.capitalizeWords}"

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedDefinitions: Set[ChapterEntry] = definingStatement.referencedDefinitions.toType[ChapterEntry]

  def withSymbol(newSymbol: String): TypeDefinition = copy(symbol = newSymbol)
  def withFormat(newFormat: Format.Explicit): TypeDefinition = copy(componentFormat = newFormat)

  @JsonSerialize
  val article: String = if (name.headOption.exists("aeiou".contains(_))) "an" else "a"
  def fullFormat: Format.Explicit = Format.Explicit(
    s"$defaultTermName is $article $name ${componentFormat.originalValue}",
    symbol +: defaultTermName +: otherComponentTypes.map(_.name),
    componentFormat.requiresBrackets,
    componentFormat.requiresComponentBrackets)

  val statementDefinition: StatementDefinition = StatementDefinition(
    symbol,
    Nil,
    TermComponent(defaultTermName, Nil) +: otherComponentTypes,
    explicitName,
    fullFormat,
    Some(definingStatement),
    None,
    Nil)
  override def inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def serializedLines: Seq[String] = Seq("type", symbol, defaultTermName, otherComponentTypes.map(_.serialized).mkString(" ").inParens).mkString(" ") +:
    (Seq(componentFormat.serialized.value) ++
      explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq ++
      Seq(Seq("definition", definingStatement.serialized.inParens).mkString(" "))
    ).indent

  override def replaceDefinition(
    oldDefinition: ExpressionDefinition,
    newDefinition: ExpressionDefinition,
    entryContext: EntryContext
  ): TypeDefinition = {
    TypeDefinition(
      symbol,
      defaultTermName,
      otherComponentTypes,
      componentFormat,
      explicitName,
      definingStatement.replaceDefinition(oldDefinition, newDefinition))
  }

  def childComponentTypesParser: Parser[Seq[ComponentType]] = {
    otherComponentTypes.map(t => Parser.singleWord.map(t.withName)).traverseParser
  }
}

object TypeDefinition extends ChapterEntryParser {
  override def name: String = "type"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      defaultTermName <- Parser.singleWord
      otherComponentTypes <- ComponentType.listWithoutBoundVariablesParser.inParens
      componentFormat <- Parser.required("format", Format.parserForTypeDefinition(otherComponentTypes))
      explicitName <- Parser.optional("name", Parser.allInParens)
      definingStatement <- Parser.required("definition", Statement.parser(ExpressionParsingContext.outsideProof(context, defaultTermName +: otherComponentTypes.ofType[TermComponent].map(_.name))).inParens)
    } yield TypeDefinition(symbol, defaultTermName, otherComponentTypes, componentFormat, explicitName, definingStatement)
  }
}
