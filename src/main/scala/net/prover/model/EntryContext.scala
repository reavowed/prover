package net.prover.model

import net.prover.books.model.{Book, EntryParsingContext}
import net.prover.entries.{BookWithContext, ChapterWithContext, EntryWithContext}
import net.prover.model.EntryContext.getStatementDefinitionFromEntry
import net.prover.model.definitions._
import net.prover.model.entries._
import net.prover.model.expressions._

case class EntryContext(entriesWithContexts: Seq[EntryWithContext])
{
  lazy val availableEntries: Seq[ChapterEntry] = entriesWithContexts.map(_.entry)
  lazy val inferencesById: Map[String, Inference] = availableEntries.flatMap(_.inferences).toMapWithKey(_.id)
  lazy val statementDefinitionsBySymbol: Map[String, StatementDefinition] = availableEntries.mapCollect(getStatementDefinitionFromEntry).toMapWithKey(_.symbol)
  lazy val termDefinitionsBySymbol: Map[String, TermDefinition] = availableEntries.ofType[TermDefinition].toMapWithKey(_.disambiguatedSymbol.serialized)
  lazy val allInferences: Seq[Inference.FromEntry] = availableEntries.flatMap(_.inferences)
  lazy val allInferenceIds: Set[String] = inferencesById.keySet
  lazy val statementDefinitions: Seq[StatementDefinition] = availableEntries.mapCollect(EntryContext.getStatementDefinitionFromEntry)
  lazy val termDefinitions: Seq[TermDefinition] = availableEntries.ofType[TermDefinition]
  lazy val typeDefinitions: Map[String, TypeDefinition] = availableEntries.ofType[TypeDefinition].map(t => t.symbol -> t).toMap
  lazy val propertyDefinitionsByType: Map[String, Seq[PropertyDefinitionOnType]] = availableEntries.ofType[PropertyDefinitionOnType].groupBy(_.parentType.symbol)
  lazy val qualifiersByType: Map[String, Seq[TypeQualifierDefinition]] = availableEntries.ofType[TypeQualifierDefinition].groupBy(_.parentType.symbol)
  lazy val relatedObjectsByType: Map[String, Seq[RelatedObjectDefinition]] = availableEntries.ofType[RelatedObjectDefinition].groupBy(_.parentType.symbol)
  lazy val standalonePropertyDefinitions: Seq[StandalonePropertyDefinition] = availableEntries.ofType[StandalonePropertyDefinition]
  lazy val typeRelationDefinitions: Seq[TypeRelationDefinition] = availableEntries.ofType[TypeRelationDefinition]
  lazy val displayShorthands: Seq[DisplayShorthand] = availableEntries.ofType[DisplayShorthand]
  lazy val writingShorthands: Seq[WritingShorthand] = availableEntries.ofType[WritingShorthand]

  lazy val deductionDefinitionOption: Option[DeductionDefinition] = {
    statementDefinitions.find(_.attributes.contains("deduction")).map(DeductionDefinition)
  }
  lazy val generalizationDefinitionOption: Option[GeneralizationDefinition] = {
    statementDefinitions.find(_.attributes.contains("generalization")).map(GeneralizationDefinition)
  }
  lazy val conjunctionDefinitionOption: Option[ConjunctionDefinition] = {
    statementDefinitions.find(_.attributes.contains("conjunction")).map(ConjunctionDefinition)
  }
  lazy val uniquenessDefinitionOption: Option[UniqueExistenceDefinition] = {
    statementDefinitions.find(_.attributes.contains("uniqueness")).map(UniqueExistenceDefinition)
  }

  lazy val typeStatementDefinitionsByType: Map[String, Seq[StatementDefinition]] = {
    typeDefinitions.mapValues { t =>
      t.statementDefinition +: (qualifiersByType.getOrElse(t.symbol, Nil) ++ propertyDefinitionsByType.getOrElse(t.symbol, Nil) ++ relatedObjectsByType.getOrElse(t.symbol, Nil)).map(_.statementDefinition)
    }
  }
  lazy val typeStatementDefinitions: Seq[StatementDefinition] = typeStatementDefinitionsByType.values.flatten.toSeq ++ typeRelationDefinitions.map(_.statementDefinition)

  def addEntry(newEntry: EntryWithContext): EntryContext = {
    EntryContext(entriesWithContexts :+ newEntry)
  }
  def addEntries(newEntries: Seq[EntryWithContext]): EntryContext = {
    EntryContext(entriesWithContexts ++ newEntries)
  }


  object RecognisedStatementDefinition {
    def unapply(symbol: String): Option[StatementDefinition] = {
      statementDefinitionsBySymbol.get(symbol)
    }
  }
  object RecognisedTermDefinition {
    def unapply(symbol: String): Option[TermDefinition] = {
      termDefinitionsBySymbol.get(symbol)
    }
  }

  object RecognisedStatementShorthand {
    def unapply(string: String): Option[DefinedStatementTemplate] = {
      writingShorthands.find(_.symbol == string).flatMap(_.template.asOptionalInstanceOf[DefinedStatementTemplate])
    }
  }
  object RecognisedTermShorthand {
    def unapply(string: String): Option[DefinedTermTemplate] = {
      writingShorthands.find(_.symbol == string).flatMap(_.template.asOptionalInstanceOf[DefinedTermTemplate])
    }
  }

  def typeDefinitionParser: Parser[TypeDefinition] = Parser.singleWord.map(typeName => typeDefinitions.getOrElse(typeName, throw new Exception(s"Unrecognised type '$typeName'")))

}

object EntryContext {

  def getStatementDefinitionFromEntry(entry: ChapterEntry): Option[StatementDefinition] = entry match {
    case statementDefinition: StatementDefinition =>
      Some(statementDefinition)
    case entryWithStatementDefinition: ChapterEntry.HasStatementDefinition =>
      Some(entryWithStatementDefinition.statementDefinition)
    case _ =>
      None
  }


  def forBooks(books: Seq[BookWithContext]): EntryContext = {
    EntryContext(books.flatMap(_.chaptersWithContexts).flatMap(_.entriesWithContexts))
  }
  def forBookExclusive(allBooks: Seq[BookWithContext], book: Book): EntryContext = {
    forBooks(Book.getDependencies(book.imports, allBooks))
  }
  def forChapterExclusive(chapterWithContext: ChapterWithContext): EntryContext = {
    import chapterWithContext._
    forBookExclusive(globalContext.booksWithContexts, book).addEntries(bookWithContext.chaptersWithContexts.takeWhile(_.chapter != chapter).flatMap(_.entriesWithContexts))
  }
  def forChapterInclusive(chapterWithContext: ChapterWithContext): EntryContext = {
    forChapterExclusive(chapterWithContext).addEntries(chapterWithContext.entriesWithContexts)
  }
  def forEntry(entryWithContext: EntryWithContext): EntryContext = {
    import entryWithContext._
    forChapterExclusive(chapterWithContext).addEntries(chapterWithContext.entriesWithContexts.takeWhile(_.entry != entry))
  }

  implicit def fromProvingContext(implicit provingContext: ProvingContext): EntryContext = provingContext.entryContext
  implicit def fromEntryParsingContext(implicit entryParsingContext: EntryParsingContext): EntryContext = entryParsingContext.entryContext
}
