package net.prover.model.entries

import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.ExpressionDefinition.ComponentType.TermComponent
import net.prover.model.definitions._
import net.prover.model.expressions.Statement

case class PropertyDefinitionOnType(
    symbol: String,
    parentTypeConditions: ParentTypeConditions,
    explicitName: Option[String],
    definingStatement: Statement)
  extends ChapterEntry.Standalone with ChapterEntry.HasOptionalExplicitName with ChapterEntry.HasStatementDefinition
{
  def parentType = parentTypeConditions.parentType

  override def title: String = s"Definition: ${name.capitalizeWords} ${parentType.name.capitalizeWords}"
  def qualifiedSymbol: String = symbol + parentType.symbol.capitalize

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.map(_.associatedChapterEntry) ++ parentTypeConditions.referencedEntries

  override def withSymbol(newSymbol: String): PropertyDefinitionOnType = copy(symbol = newSymbol)
  override def withName(newName: Option[String]): PropertyDefinitionOnType = copy(explicitName = newName)

  def baseFormat = Format.Explicit(s"%1 is %0", s"${parentType.mainVariableDefinition.name} is $name", 2, true, true)
  def fullFormat = parentType.defaultQualifier.prependFormat(baseFormat)

  val allVariableDefinitions: Seq[SimpleVariableDefinition] = parentTypeConditions.allVariableDefinitions
  val allVariableNames: Seq[String] = allVariableDefinitions.map(_.name)
  def allComponents: Seq[TermComponent] = allVariableNames.map(ComponentType.TermComponent(_, Nil))

  val statementDefinition: StatementDefinition = StatementDefinition.Derived(
    qualifiedSymbol,
    allComponents,
    Some(name),
    fullFormat,
    Some(parentTypeConditions.parentConditionConstructor(definingStatement, 0)),
    this)

  override val inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def serializedLines: Seq[String] = Seq("property", symbol, "on", parentType.symbol).mkString(" ") +:
    (parentTypeConditions.serializedFollowingLines ++
      explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq ++
      Seq(Seq("definition", definingStatement.serialized.inParens).mkString(" "))
    ).indent

  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryContext: EntryContext
  ): PropertyDefinitionOnType = {
    PropertyDefinitionOnType(
      symbol,
      parentTypeConditions.replaceDefinitions(entryReplacements, expressionDefinitionReplacements, entryContext),
      explicitName,
      definingStatement.replaceDefinitions(expressionDefinitionReplacements))
  }
}

object PropertyDefinitionOnType extends ChapterEntryParser {
  override def name: String = "property"
  override def parser(implicit context: EntryParsingContext): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      parentTypeConditions <- Parser.required("on", ParentTypeConditions.parser)
      explicitName <- Parser.optional("name", Parser.allInParens)
      expressionParsingContext = parentTypeConditions.requiredParentObjects.addParametersToParsingContext(ExpressionParsingContext.forTypeDefinition(parentTypeConditions.allVariableDefinitions))
      definingStatement <- Parser.required("definition", Statement.parser(expressionParsingContext).inParens)
    } yield PropertyDefinitionOnType(symbol, parentTypeConditions, explicitName, definingStatement)
  }
}
