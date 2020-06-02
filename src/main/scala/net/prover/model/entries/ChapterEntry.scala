package net.prover.model.entries

import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.model.definitions.{ExpressionDefinition, StatementDefinition}
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
  trait HasDefaultTermName extends ChapterEntry {
    def defaultTermName: String
    def withDefaultTermName(newDefaultTermName: String): ChapterEntry
  }
  trait HasOptionalExplicitName extends ChapterEntry with HasSymbol {
    def explicitName: Option[String]
    override def name: String = explicitName.getOrElse(symbol)
    def withName(newName: Option[String]): ChapterEntry
  }
  trait HasStatementDefinition extends ChapterEntry {
    def statementDefinition: StatementDefinition
  }
  trait HasArticle extends ChapterEntry {
    @JsonSerialize
    val article: String = if (name.headOption.exists("aeiou".contains(_))) "an" else "a"
  }

  val parsers: Seq[ChapterEntryParser] = Seq(
    Comment,
    StatementDefinitionEntry,
    TermDefinitionEntry,
    TypeDefinition,
    TypeQualifierDefinition,
    PropertyDefinitionOnType,
    RelatedObjectDefinition,
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
