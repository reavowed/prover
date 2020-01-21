package net.prover.model.entries

import net.prover.model._
import net.prover.model.entries.ExpressionDefinition.ComponentType
import net.prover.model.expressions.Statement

case class PropertyDefinition(
    symbol: String,
    parentType: TypeDefinition,
    defaultTermName: String,
    parentComponentTypes: Seq[ComponentType],
    explicitName: Option[String],
    definingStatement: Statement)
  extends ChapterEntry.Standalone
{
  override def name: String = explicitName.getOrElse(symbol)
  override def title: String = s"Definition: ${name.capitalize} ${parentType.name.capitalize}"
  def qualifiedSymbol: String = symbol + parentType.symbol.capitalize

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedDefinitions: Set[ChapterEntry] = definingStatement.referencedDefinitions.toType[ChapterEntry] + parentType

  def fullFormat: Format = Format.Explicit(s"$defaultTermName is $name", Seq(defaultTermName), requiresBrackets = false, requiresComponentBrackets = true)
  def statementDefinition = StatementDefinition(
    qualifiedSymbol,
    Nil,
    ComponentType.TermComponent(defaultTermName) +: parentComponentTypes,
    explicitName.orElse(Some(symbol)),
    fullFormat,
    Some(definingStatement),
    None,
    Nil)
  override def inferences: Seq[Inference] = statementDefinition.inferences

  override def serializedLines: Seq[String] = (Seq("property", symbol, "on", parentType.name, defaultTermName) ++ parentComponentTypes.map(_.serialized)).mkString(" ") +:
    (explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq ++
      Seq(Seq("definition", definingStatement.serialized.inParens).mkString(" "))
    ).indent

  override def replaceDefinition(
    oldDefinition: ExpressionDefinition,
    newDefinition: ExpressionDefinition,
    entryContext: EntryContext
  ): PropertyDefinition = {
    PropertyDefinition(
      symbol,
      entryContext.typeDefinitions.find(_.symbol == parentType.symbol).get,
      defaultTermName,
      parentComponentTypes,
      explicitName,
      definingStatement.replaceDefinition(oldDefinition, newDefinition))
  }
}

object PropertyDefinition extends ChapterEntryParser {
  override def name: String = "property"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      parentTypeName <- Parser.required("on", Parser.singleWord)
      parentType = context.typeDefinitions.find(_.name == parentTypeName).getOrElse(throw new Exception(s"Unrecognised type '$parentTypeName'"))
      defaultSymbol <- Parser.singleWord
      parentComponentTypes <- parentType.childComponentTypesParser
      explicitName <- Parser.optional("name", Parser.allInParens)
      definingStatement <- Parser.required("definition", Statement.parser(ExpressionParsingContext.outsideProof(context, defaultSymbol +: parentComponentTypes.ofType[ComponentType.TermComponent].map(_.name))).inParens)
    } yield PropertyDefinition(symbol, parentType, defaultSymbol, parentComponentTypes, explicitName, definingStatement)
  }

}
