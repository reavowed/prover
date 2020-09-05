package net.prover.model.entries

import net.prover._
import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.{ExpressionDefinition, StatementDefinition}
import net.prover.model.expressions.{DefinedStatement, FunctionParameter, Statement, TermVariable}

case class RelatedObjectDefinition(
    symbol: String,
    mainVariableDefinition: SimpleVariableDefinition,
    parentTypeConditions: ParentTypeConditions,
    explicitName: Option[String],
    definingStatement: Statement)
  extends ChapterEntry.Standalone with ChapterEntry.HasOptionalExplicitName with ChapterEntry.HasStatementDefinition with ChapterEntry.HasArticle
{
  def parentType: TypeDefinition = parentTypeConditions.parentType
  override def title: String = s"Definition: ${name.capitalizeWords} for ${parentType.name.capitalizeWords}"
  def qualifiedSymbol: String = symbol + parentType.symbol.capitalize

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.map(_.associatedChapterEntry) ++ parentTypeConditions.referencedEntries

  override def withSymbol(newSymbol: String): RelatedObjectDefinition = copy(symbol = newSymbol)
  override def withName(newName: Option[String]): ChapterEntry = copy(explicitName = newName)

  def baseFormat: Format.Explicit = Format.Explicit(s"%1 is $article %0 for %2", s"${mainVariableDefinition.name} is $article $name for ${parentType.mainVariableDefinition.name}", 3, true, true)
  def fullFormat: Format = parentType.defaultQualifier.prependFormat(baseFormat)
  def parentVariableDefinitions: Seq[SimpleVariableDefinition] = parentTypeConditions.allVariableDefinitions
  val statementDefinition: StatementDefinition = StatementDefinition.Derived(
    qualifiedSymbol,
    (mainVariableDefinition +: parentVariableDefinitions).map(_.name).map(ComponentType.TermComponent(_, Nil)),
    Some(name),
    fullFormat,
    Some(parentTypeConditions.parentConditionConstructor(definingStatement, 1)),
    this)
  override val inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  def condition(offset: Int): DefinedStatement = {
    statementDefinition(FunctionParameter(0, 0) +: parentVariableDefinitions.indices.map(i => TermVariable(i + offset)): _*)
  }

  override def replaceDefinitions(entryReplacements: Map[ChapterEntry, ChapterEntry], expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition], entryContext: EntryContext): ChapterEntry = {
    RelatedObjectDefinition(
      symbol,
      mainVariableDefinition,
      parentTypeConditions.replaceDefinitions(entryReplacements, expressionDefinitionReplacements, entryContext),
      explicitName,
      definingStatement.replaceDefinitions(expressionDefinitionReplacements))
  }

  override def serializedLines: Seq[String] = Seq(RelatedObjectDefinition.name, symbol, mainVariableDefinition.serialized, "for", parentType.symbol).mkString(" ") +:
    (parentTypeConditions.serializedFollowingLines ++
      explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq ++
      Seq(Seq("definition", definingStatement.serialized.inParens).mkString(" "))
    ).indent
}

object RelatedObjectDefinition extends ChapterEntryParser {
  override def name: String = "relatedObject"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      mainVariableDefinition <- SimpleVariableDefinition.parser
      parentTypeConditions <- Parser.required("for", ParentTypeConditions.parser)
      explicitName <- Parser.optional("name", Parser.allInParens)
      expressionParsingContext = parentTypeConditions.requiredParentObjects.addParametersToParsingContext(ExpressionParsingContext.forTypeDefinition(mainVariableDefinition +: parentTypeConditions.allVariableDefinitions))
      definingStatement <- Parser.required("definition", Statement.parser(expressionParsingContext).inParens)
    } yield RelatedObjectDefinition(symbol, mainVariableDefinition, parentTypeConditions, explicitName, definingStatement)
  }
}
