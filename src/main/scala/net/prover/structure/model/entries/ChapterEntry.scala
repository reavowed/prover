package net.prover.structure.model.entries

import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model.definitions.{CompoundExpressionDefinition, CompoundStatementDefinition}
import net.prover.model.{Inference, Parser, SimpleVariableDefinition}
import net.prover.shorthands.model.entries._
import net.prover.structure.EntryContext
import net.prover.structure.parsers.ChapterEntryParser
import net.prover.types.model.entries._

trait ChapterEntry {
  @JsonSerialize
  def name: String
  def inferences: Seq[Inference.FromEntry]
  def serializedLines: Seq[String]
  def referencedInferenceIds: Set[String]
  def referencedEntries: Set[ChapterEntry]

  def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[CompoundExpressionDefinition, CompoundExpressionDefinition],
    entryContext: EntryContext
  ): ChapterEntry

  def validate(): Unit = {}
}

object ChapterEntry {
  trait Standalone extends ChapterEntry {
    @JsonSerialize
    def title: String
  }

  trait HasSymbol extends ChapterEntry {
    def symbol: String
    def withSymbol(newSymbol: String): ChapterEntry
  }
  trait HasParentType extends ChapterEntry {
    def parentType: TypeDefinition
  }
  trait HasChangeableName extends ChapterEntry {
    def withName(newName: String): ChapterEntry
  }
  trait HasMainVariable extends ChapterEntry {
    def mainVariableDefinition: SimpleVariableDefinition
    def withMainVariableDefinition(newMainVariableDefinition: SimpleVariableDefinition): ChapterEntry
  }
  trait HasOptionalExplicitName extends ChapterEntry with HasSymbol {
    def explicitName: Option[String]
    override def name: String = explicitName.getOrElse(symbol)
    def withName(newName: Option[String]): ChapterEntry
  }
  trait HasStatementDefinition extends ChapterEntry {
    def statementDefinition: CompoundStatementDefinition
  }
  trait HasArticle extends ChapterEntry {
    @JsonSerialize
    val article: String = if (name.headOption.exists("aeiou".contains(_))) "an" else "a"
  }

  val parsers: Seq[ChapterEntryParser] = Seq(
    Comment,
    CompoundStatementDefinitionEntry,
    CompoundTermDefinitionEntry,
    TypeDefinition,
    TypeQualifierDefinition,
    PropertyDefinitionOnType,
    RelatedObjectDefinition,
    TypeRelationDefinition,
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
