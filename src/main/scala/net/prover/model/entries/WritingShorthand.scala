package net.prover.model.entries

import net.prover.entries.{EntryParsingContext, EntryWithContext}
import net.prover.model.{ExpressionParsingContext, Inference}
import net.prover.model.definitions.ExpressionDefinition
import net.prover.model.expressions.{Statement, Template, Term}
import net.prover.parsing.{KnownWordParser, Parser}

case class WritingShorthand(template: Template, symbol: String) extends ChapterEntry {
  override def name: String = symbol
  override def serializedLines: Seq[String] = Seq(Seq("write", template.serialized, "as", symbol).mkString(" "))
  override def inferences: Seq[Inference.FromEntry] = Nil
  override def referencedEntries: Set[ChapterEntry] = template.referencedDefinitions.map(_.associatedChapterEntry)
  override def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryWithContext: EntryWithContext
  ): WritingShorthand = copy(template = template.replaceDefinitions(expressionDefinitionReplacements))
}

object WritingShorthand extends ChapterEntryParser {
  override def name: String = "write"
  override def parser(implicit entryParsingContext: EntryParsingContext): Parser[WritingShorthand] =
    for {
      template <- Template.parser
      symbol <- Parser.required("as", Parser.singleWord)
    } yield WritingShorthand(template, symbol)

  def statementParser(implicit context: ExpressionParsingContext): KnownWordParser[Statement] = {
    context.availableEntries.statementShorthandParser
      .flatMap(_.expressionParser.map(_.asInstanceOf[Statement]))
  }
  def termParser(implicit context: ExpressionParsingContext): KnownWordParser[Term] = {
    context.availableEntries.termShorthandParser
      .flatMap(_.expressionParser.map(_.asInstanceOf[Term]))
  }
}
