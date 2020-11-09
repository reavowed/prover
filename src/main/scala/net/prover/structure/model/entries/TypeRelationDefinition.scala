package net.prover.structure.model.entries

import net.prover._
import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.ExpressionDefinition.ComponentType.TermComponent
import net.prover.model.definitions.{ConjunctionDefinition, ExpressionDefinition, StatementDefinition}
import net.prover.model.expressions.{Statement, TermVariable}
import net.prover.structure.EntryContext
import net.prover.structure.model.parsers.ChapterEntryParser

case class TypeRelationDefinition(
    symbol: String,
    firstType: TypeDefinition,
    secondType: TypeDefinition,
    firstVariable: SimpleVariableDefinition,
    secondVariable: SimpleVariableDefinition,
    linkingPhrase: String,
    explicitName: Option[String],
    definingStatement: Statement,
    conjunctionDefinition: ConjunctionDefinition)
  extends ChapterEntry.Standalone with ChapterEntry.HasOptionalExplicitName with ChapterEntry.HasStatementDefinition
{
  override val title: String = s"Definition: ${name.capitalizeWords}"

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.map(_.associatedChapterEntry) + conjunctionDefinition.referencedEntry + firstType + secondType

  override def withSymbol(newSymbol: String): ChapterEntry = copy(symbol = newSymbol)
  override def withName(newName: Option[String]): ChapterEntry = copy(explicitName = newName)

  def allVariableDefinitions: Seq[SimpleVariableDefinition] = Seq(firstVariable, secondVariable)
  def allVariableNames: Seq[String] = allVariableDefinitions.map(_.name)
  def allComponents: Seq[TermComponent] = allVariableNames.map(ComponentType.TermComponent(_, Nil))

  def format = Format.Explicit(s"%1 $linkingPhrase %2", s"${firstVariable.name} $linkingPhrase ${secondVariable.name}", 3, true, true)

  override def validate(): Unit = {
    if (firstType.defaultQualifier.isDefined) throw new Exception("Cannot relate a type with a default qualifier")
    if (secondType.defaultQualifier.isDefined) throw new Exception("Cannot relate a type with a default qualifier")
  }

  override val statementDefinition: StatementDefinition = StatementDefinition.Derived(
    symbol,
    allComponents,
    Some(name),
    format,
    Some(conjunctionDefinition(firstType.statementDefinition(TermVariable(0, Nil)), conjunctionDefinition(secondType.statementDefinition(TermVariable(1, Nil)), definingStatement))),
    this)
  override val inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def serializedLines: Seq[String] = Seq("typeRelation", symbol, linkingPhrase.inParens, firstType.symbol, firstVariable.serialized, secondType.symbol, secondVariable.serialized).mkString(" ") +:
    ( explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq :+
      Seq("definition", definingStatement.serialized.inParens).mkString(" ")).indent

  override def replaceDefinitions(entryReplacements: Map[ChapterEntry, ChapterEntry], expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition], entryContext: EntryContext): ChapterEntry = {
    TypeRelationDefinition(
      symbol,
      entryReplacements(firstType).asInstanceOf[TypeDefinition],
      entryReplacements(secondType).asInstanceOf[TypeDefinition],
      firstVariable,
      secondVariable,
      linkingPhrase,
      explicitName,
      definingStatement.replaceDefinitions(expressionDefinitionReplacements),
      entryContext.conjunctionDefinitionOption.get)
  }
}


object TypeRelationDefinition extends ChapterEntryParser {
  override def name: String = "typeRelation"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      linkingPhrase <- Parser.allInParens
      firstType <- context.typeDefinitionParser
      firstVariableDefinition <- SimpleVariableDefinition.parser
      secondType <- context.typeDefinitionParser
      secondVariableDefinition <- SimpleVariableDefinition.parser
      explicitName <- Parser.optional("name", Parser.allInParens)
      expressionParsingContext = ExpressionParsingContext.forTypeDefinition(Seq(firstVariableDefinition, secondVariableDefinition))
      definingStatement <- Parser.required("definition", Statement.parser(expressionParsingContext).inParens)
      conjunctionDefinition = context.conjunctionDefinitionOption.getOrElse(throw new Exception("Cannot create type qualifier definition without conjunction"))
    } yield TypeRelationDefinition(symbol, firstType, secondType, firstVariableDefinition, secondVariableDefinition, linkingPhrase, explicitName, definingStatement, conjunctionDefinition)
  }
}
