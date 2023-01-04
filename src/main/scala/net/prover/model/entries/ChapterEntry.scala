package net.prover.model.entries

import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.books.model.EntryParsingContext
import net.prover.model.definitions.{ExpressionDefinition, StatementDefinition}
import net.prover.model.expressions.Statement
import net.prover.model.{EntryContext, ExpressionParsingContext, Inference, Parser, SimpleVariableDefinition}

trait ChapterEntry {
  @JsonSerialize
  def name: String
  def inferences: Seq[Inference.FromEntry]
  def serializedLines: Seq[String]
  def referencedInferenceIds: Set[String]
  def referencedEntries: Set[ChapterEntry]

  def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
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
  trait HasDefiningStatement extends ChapterEntry {
    def definingStatement: Statement
    def withDefiningStatement(newDefiningStatement: Statement): ChapterEntry
    def definingStatementParsingContext(implicit entryContext: EntryContext): ExpressionParsingContext
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
    TypeRelationDefinition,
    StandalonePropertyDefinition,
    Axiom,
    Theorem,
    DisplayShorthand,
    WritingShorthand)

  def parser(context: EntryParsingContext): Parser[Option[ChapterEntry]] = {
    Parser.singleWordIfAny.flatMapFlatMapReverse { entryType =>
      parsers.find(_.name == entryType).map(_.parser(context))
    }
  }
}
