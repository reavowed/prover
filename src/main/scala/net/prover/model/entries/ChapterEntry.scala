package net.prover.model.entries

import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model.definitions.ExpressionDefinition
import net.prover.model.{EntryContext, Inference, Parser}

trait ChapterEntry {
  @JsonSerialize
  def name: String
  def inferences: Seq[Inference.FromEntry] = Nil
  def serializedLines: Seq[String]
  def referencedInferenceIds: Set[String]
  def referencedEntries: Set[ChapterEntry]

  def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryContext: EntryContext
  ): ChapterEntry
}

object ChapterEntry {
  trait Standalone extends ChapterEntry {
    @JsonSerialize
    def title: String
  }

  trait CanChangeName extends ChapterEntry {
    def withName(newName: String): ChapterEntry
  }
  trait CanChangeOptionalName extends ChapterEntry {
    def withName(newName: Option[String]): ChapterEntry
  }

  val parsers: Seq[ChapterEntryParser] = Seq(
    Comment,
    StatementDefinitionEntry,
    TermDefinitionEntry,
    TypeDefinition,
    TypeQualifierDefinition,
    PropertyDefinitionOnType,
    StandalonePropertyDefinition,
    Axiom,
    Theorem,
    DisplayShorthand,
    WritingShorthand)

  def parser(context: EntryContext): Parser[Option[ChapterEntry]] = {
    Parser.singleWordIfAny.flatMapFlatMapReverse { entryType =>
      parsers.find(_.name == entryType).map(_.parser(context))
    }
  }
}
