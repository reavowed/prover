package net.prover.model.entries

import net.prover.model._
import net.prover.model.entries.ExpressionDefinition.ComponentType
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
  override def name: String = explicitName.getOrElse(symbol)
  override def title: String = s"Definition: ${name.capitalizeWords}"

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedDefinitions: Set[ChapterEntry] = definingStatement.referencedDefinitions.toType[ChapterEntry]

  def article: String = if (name.headOption.exists("aeiou".contains(_))) "an" else "a"
  def fullFormat: Format.Explicit = Format.Explicit(
    s"$defaultTermName is $article $name ${componentFormat.originalValue}",
    defaultTermName +: otherComponentTypes.map(_.name),
    componentFormat.requiresBrackets,
    componentFormat.requiresComponentBrackets)

  def statementDefinition = StatementDefinition(
    symbol,
    Nil,
    ComponentType.TermComponent(defaultTermName) +: otherComponentTypes,
    explicitName,
    fullFormat,
    Some(definingStatement),
    None,
    Nil)
  override def inferences: Seq[Inference] = statementDefinition.inferences

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
      componentFormat <- Parser.required("format", Format.parser(otherComponentTypes.map(_.name)))
      explicitName <- Parser.optional("name", Parser.allInParens)
      definingStatement <- Parser.required("definition", Statement.parser(ExpressionParsingContext.outsideProof(context, defaultTermName +: otherComponentTypes.ofType[ComponentType.TermComponent].map(_.name))).inParens)
    } yield TypeDefinition(symbol, defaultTermName, otherComponentTypes, componentFormat, explicitName, definingStatement)
  }
}
