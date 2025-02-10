package net.prover.model.entries

import net.prover.entries.{EntryParsingContext, EntryWithContext}
import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.ExpressionDefinition.ComponentType.TermComponent
import net.prover.model.definitions.{ExpressionDefinition, Qualifier, StatementDefinition}
import net.prover.model.expressions.{Statement, TermVariable}
import net.prover.parsing.Parser
import net.prover.proving.structure.definitions.ConjunctionDefinition

case class TypeQualifierDefinition(
    symbol: String,
    parentType: TypeDefinition,
    qualifier: Qualifier,
    explicitName: Option[String],
    definingStatement: Statement,
    conjunctionDefinition: ConjunctionDefinition)
  extends ChapterEntry.Standalone with ChapterEntry.HasOptionalExplicitName with ChapterEntry.HasStatementDefinition
{
  override def title: String = s"Definition: ${parentType.name.capitalizeWords} ${name.capitalizeWords}"
  def qualifiedSymbol: String = parentType.symbol + symbol.capitalize

  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.map(_.associatedChapterEntry) + conjunctionDefinition.referencedEntry + parentType

  override def withSymbol(newSymbol: String): TypeQualifierDefinition = copy(symbol = newSymbol)
  def withName(newName: Option[String]): TypeQualifierDefinition = copy(explicitName = newName)

  def fullFormat: Format = qualifier.prependFormat(Format.Explicit(s"%1 is", s"${parentType.mainVariableDefinition} is", 2, true, true))
  def allVariableDefinitions: Seq[SimpleVariableDefinition] = parentType.mainVariableDefinition +: qualifier.variableDefinitions
  def allVariableNames: Seq[String] = allVariableDefinitions.map(_.name)
  def allComponents: Seq[TermComponent] = allVariableNames.map(ComponentType.TermComponent(_, Nil))
  val statementDefinition: StatementDefinition = StatementDefinition.Derived(
    qualifiedSymbol,
    allComponents,
    Some(name),
    fullFormat,
    Some(conjunctionDefinition(parentType.statementDefinition(parentType.allVariableNames.indices.map(TermVariable(_, Nil)): _*), definingStatement)),
    this)

  override val inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def serializedLines: Seq[String] = Seq("qualifier", symbol, "on", parentType.symbol, qualifier.serialized).mkString(" ") +:
    (explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq ++
      Seq(Seq("definition", definingStatement.serialized.inParens).mkString(" "))
    ).indent

  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryWithContext: EntryWithContext
  ): TypeQualifierDefinition = {
    TypeQualifierDefinition(
      symbol,
      entryReplacements(parentType).asInstanceOf[TypeDefinition],
      qualifier,
      explicitName,
      definingStatement.replaceDefinitions(expressionDefinitionReplacements),
      entryWithContext.availableEntries.conjunctionDefinitionOption.get)
  }
}

object TypeQualifierDefinition extends ChapterEntryParser {
  override def name: String = "qualifier"
  override def parser(implicit entryParsingContext: EntryParsingContext): Parser[TypeQualifierDefinition] = {
    import entryParsingContext.availableEntries
    for {
      symbol <- Parser.singleWord
      parentType <- Parser.required("on", availableEntries.typeDefinitionParser)
      qualifier <- Qualifier.parser
      expressionParsingContext = ExpressionParsingContext.forTypeDefinition(parentType.mainVariableDefinition +: qualifier.variableDefinitions)
      explicitName <- Parser.optional("name", Parser.allInParens)
      definingStatement <- Parser.required("definition", Statement.parser(expressionParsingContext).inParens)
      conjunctionDefinition = availableEntries.conjunctionDefinitionOption.getOrElse(throw new Exception("Cannot create type qualifier definition without conjunction"))
    } yield TypeQualifierDefinition(symbol, parentType, qualifier, explicitName, definingStatement, conjunctionDefinition)
  }
}
