package net.prover.types.model.entries

import net.prover._
import net.prover.extensions.ExpressionExtensions._
import net.prover.model._
import net.prover.model.definitions.CompoundExpressionDefinition.ComponentType
import net.prover.model.definitions.{CompoundExpressionDefinition, CompoundStatementDefinition}
import net.prover.model.expressions.Statement
import net.prover.structure.EntryContext
import net.prover.structure.model.entries.ChapterEntry
import net.prover.structure.parsers.ChapterEntryParser

case class StandalonePropertyDefinition(
    symbol: String,
    mainVariableDefinition: SimpleVariableDefinition,
    explicitName: Option[String],
    definingStatement: Statement)
  extends ChapterEntry.Standalone with ChapterEntry.HasOptionalExplicitName with ChapterEntry.HasStatementDefinition
{
  override def title: String = s"Definition: ${name.capitalizeWords}"
  def qualifiedSymbol: String = symbol

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.map(_.associatedChapterEntry)

  override def withSymbol(newSymbol: String): StandalonePropertyDefinition = copy(symbol = newSymbol)
  override def withName(newName: Option[String]): ChapterEntry = copy(explicitName = newName)

  def fullFormat: Format = Format.Explicit(s"%1 is %0", s"${mainVariableDefinition.name} is $name", 2, requiresBrackets = false, requiresComponentBrackets = true)
  val statementDefinition: CompoundStatementDefinition = CompoundStatementDefinition.Derived(
    qualifiedSymbol,
    Seq(ComponentType.TermComponent(mainVariableDefinition.name, Nil)),
    explicitName.orElse(Some(symbol)),
    fullFormat,
    Some(definingStatement),
    this)
  override val inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def serializedLines: Seq[String] = Seq(StandalonePropertyDefinition.name, symbol, mainVariableDefinition.serialized).mkString(" ") +:
    (explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq ++
      Seq(Seq("definition", definingStatement.serialized.inParens).mkString(" "))
    ).indent

  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[CompoundExpressionDefinition, CompoundExpressionDefinition],
    entryContext: EntryContext
  ): StandalonePropertyDefinition = {
    StandalonePropertyDefinition(
      symbol,
      mainVariableDefinition,
      explicitName,
      definingStatement.replaceDefinitions(expressionDefinitionReplacements))
  }
}

object StandalonePropertyDefinition extends ChapterEntryParser {
  override def name: String = "standaloneProperty"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      mainVariableDefinition <- SimpleVariableDefinition.parser
      expressionParsingContext = ExpressionParsingContext.forTypeDefinition(Seq(mainVariableDefinition))
      explicitName <- Parser.optional("name", Parser.allInParens)
      definingStatement <- Parser.required("definition", Statement.parser(expressionParsingContext).inParens)
    } yield StandalonePropertyDefinition(symbol, mainVariableDefinition, explicitName, definingStatement)
  }
}


