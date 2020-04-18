package net.prover.model.entries

import net.prover.model._
import net.prover.model.definitions.ExpressionDefinition.ComponentType
import net.prover.model.definitions.{ExpressionDefinition, StatementDefinition}
import net.prover.model.expressions.Statement

case class StandalonePropertyDefinition(
    symbol: String,
    defaultTermName: String,
    explicitName: Option[String],
    definingStatement: Statement)
  extends ChapterEntry.Standalone
{
  override def name: String = explicitName.getOrElse(symbol)
  override def title: String = s"Definition: ${name.capitalize}"
  def qualifiedSymbol: String = symbol

  override def referencedInferenceIds: Set[String] = Set.empty
  override def referencedEntries: Set[ChapterEntry] = definingStatement.referencedDefinitions.map(_.associatedChapterEntry)

  def fullFormat: Format = Format.Explicit(s"$defaultTermName is $name", Seq(defaultTermName), requiresBrackets = false, requiresComponentBrackets = true)
  val statementDefinition: StatementDefinition = StatementDefinition.Derived(
    qualifiedSymbol,
    Seq(ComponentType.TermComponent(defaultTermName, Nil)),
    explicitName.orElse(Some(symbol)),
    fullFormat,
    Some(definingStatement),
    this)
  override val inferences: Seq[Inference.FromEntry] = statementDefinition.inferences

  override def serializedLines: Seq[String] = Seq(StandalonePropertyDefinition.name, symbol, defaultTermName).mkString(" ") +:
    (explicitName.map(n => Seq("name", n.inParens).mkString(" ")).toSeq ++
      Seq(Seq("definition", definingStatement.serialized.inParens).mkString(" "))
    ).indent

  override def replaceDefinition(
    oldDefinition: ExpressionDefinition,
    newDefinition: ExpressionDefinition,
    entryContext: EntryContext
  ): StandalonePropertyDefinition = {
    StandalonePropertyDefinition(
      symbol,
      defaultTermName,
      explicitName,
      definingStatement.replaceDefinition(oldDefinition, newDefinition))
  }
}

object StandalonePropertyDefinition extends ChapterEntryParser {
  override def name: String = "standaloneProperty"
  override def parser(implicit context: EntryContext): Parser[ChapterEntry] = {
    for {
      symbol <- Parser.singleWord
      defaultTermName <- Parser.singleWord
      explicitName <- Parser.optional("name", Parser.allInParens)
      definingStatement <- Parser.required("definition", Statement.parser(ExpressionParsingContext.outsideProof(context, Seq(defaultTermName))).inParens)
    } yield StandalonePropertyDefinition(symbol, defaultTermName, explicitName, definingStatement)
  }
}


