package net.prover.model.entries

import com.fasterxml.jackson.databind.annotation.JsonSerialize
import net.prover.books.keys.WithKeyProperty
import net.prover.entries.{EntryParsingContext, EntryWithContext}
import net.prover.model.definitions.{ExpressionDefinition, StatementDefinition}
import net.prover.model.expressions.Statement
import net.prover.model.{AvailableEntries, ExpressionParsingContext, Inference, SimpleVariableDefinition}
import net.prover.parsing.Parser

trait ChapterEntry {
  @JsonSerialize
  def name: String
  def inferences: Seq[Inference.FromEntry]
  def serializedLines: Seq[String]
  def referencedEntries: Set[ChapterEntry]

  def replaceDefinitions(
    entryReplacements: Map[ChapterEntry, ChapterEntry],
    expressionDefinitionReplacements: Map[ExpressionDefinition, ExpressionDefinition],
    entryWithContext: EntryWithContext
  ): ChapterEntry

  def validate(): Unit = {}
}

object ChapterEntry {
  implicit val keyProperty: WithKeyProperty[ChapterEntry] = _.name

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
    def definingStatementParsingContext(implicit availableEntries: AvailableEntries): ExpressionParsingContext
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

  def parser(implicit entryParsingContext: EntryParsingContext): Parser[Option[ChapterEntry]] = {
    Parser.singleWordIfAny.flatMapFlatMapReverse { entryType =>
      parsers.find(_.name == entryType).map(_.parser)
    }
  }
}
