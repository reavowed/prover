package net.prover.model

import net.prover.books.model.Book
import net.prover.entries.{BookWithContext, ChapterWithContext, EntryParsingContext, EntryWithContext}
import net.prover.model.AvailableEntries.getStatementDefinitionFromEntry
import net.prover.model.definitions._
import net.prover.model.entries._
import net.prover.model.expressions._

case class AvailableEntries(entriesWithContexts: Seq[EntryWithContext])
{
  lazy val allEntries: Seq[ChapterEntry] = entriesWithContexts.map(_.entry)
  lazy val inferencesById: Map[String, Inference] = allEntries.flatMap(_.inferences).toMapWithKey(_.id)
  lazy val statementDefinitionsBySymbol: Map[String, StatementDefinition] = allEntries.mapCollect(getStatementDefinitionFromEntry).toMapWithKey(_.symbol)
  lazy val termDefinitionsBySymbol: Map[String, TermDefinition] = allEntries.ofType[TermDefinition].toMapWithKey(_.disambiguatedSymbol.serialized)
  lazy val allInferencesWithContext: Seq[(Inference.FromEntry, EntryWithContext)] = for {
    entryWithContext <- entriesWithContexts
    inference <- entryWithContext.entry.inferences
  } yield (inference, entryWithContext)
  lazy val allInferences: Seq[Inference.FromEntry] = allInferencesWithContext.map(_._1)
  lazy val allInferenceIds: Set[String] = inferencesById.keySet
  lazy val statementDefinitions: Seq[StatementDefinition] = allEntries.mapCollect(AvailableEntries.getStatementDefinitionFromEntry)
  lazy val termDefinitions: Seq[TermDefinition] = allEntries.ofType[TermDefinition]
  lazy val typeDefinitions: Map[String, TypeDefinition] = allEntries.ofType[TypeDefinition].map(t => t.symbol -> t).toMap
  lazy val propertyDefinitionsByType: Map[String, Seq[PropertyDefinitionOnType]] = allEntries.ofType[PropertyDefinitionOnType].groupBy(_.parentType.symbol)
  lazy val qualifiersByType: Map[String, Seq[TypeQualifierDefinition]] = allEntries.ofType[TypeQualifierDefinition].groupBy(_.parentType.symbol)
  lazy val relatedObjectsByType: Map[String, Seq[RelatedObjectDefinition]] = allEntries.ofType[RelatedObjectDefinition].groupBy(_.parentType.symbol)
  lazy val standalonePropertyDefinitions: Seq[StandalonePropertyDefinition] = allEntries.ofType[StandalonePropertyDefinition]
  lazy val typeRelationDefinitions: Seq[TypeRelationDefinition] = allEntries.ofType[TypeRelationDefinition]
  lazy val displayShorthands: Seq[DisplayShorthand] = allEntries.ofType[DisplayShorthand]
  lazy val writingShorthands: Seq[WritingShorthand] = allEntries.ofType[WritingShorthand]

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
    typeDefinitions.view.mapValues { t =>
      t.statementDefinition +: (qualifiersByType.getOrElse(t.symbol, Nil) ++ propertyDefinitionsByType.getOrElse(t.symbol, Nil) ++ relatedObjectsByType.getOrElse(t.symbol, Nil)).map(_.statementDefinition)
    }.toMap
  }
  lazy val typeStatementDefinitions: Seq[StatementDefinition] = typeStatementDefinitionsByType.values.flatten.toSeq ++ typeRelationDefinitions.map(_.statementDefinition)

  def addEntry(newEntry: EntryWithContext): AvailableEntries = {
    AvailableEntries(entriesWithContexts :+ newEntry)
  }
  def addEntries(newEntries: Seq[EntryWithContext]): AvailableEntries = {
    AvailableEntries(entriesWithContexts ++ newEntries)
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

object AvailableEntries {

  def getStatementDefinitionFromEntry(entry: ChapterEntry): Option[StatementDefinition] = entry match {
    case statementDefinition: StatementDefinition =>
      Some(statementDefinition)
    case entryWithStatementDefinition: ChapterEntry.HasStatementDefinition =>
      Some(entryWithStatementDefinition.statementDefinition)
    case _ =>
      None
  }


  def forBooks(books: Seq[BookWithContext]): AvailableEntries = {
    AvailableEntries(books.flatMap(_.chaptersWithContexts).flatMap(_.entriesWithContexts))
  }
  def forBookExclusive(allBooks: Seq[BookWithContext], bookWithContext: BookWithContext): AvailableEntries = {
    forBooks(Book.getDependencies(bookWithContext.book.imports, allBooks))
  }
  def forChapterExclusive(chapterWithContext: ChapterWithContext): AvailableEntries = {
    import chapterWithContext._
    forBookExclusive(globalContext.booksWithContexts, bookWithContext).addEntries(bookWithContext.chaptersWithContexts.takeWhile(_.chapter != chapter).flatMap(_.entriesWithContexts))
  }
  def forChapterInclusive(chapterWithContext: ChapterWithContext): AvailableEntries = {
    forChapterExclusive(chapterWithContext).addEntries(chapterWithContext.entriesWithContexts)
  }
  def forEntry(entryWithContext: EntryWithContext): AvailableEntries = {
    import entryWithContext._
    forChapterExclusive(chapterWithContext).addEntries(chapterWithContext.entriesWithContexts.takeWhile(_.entry != entry))
  }

  implicit def fromProvingContext(implicit provingContext: ProvingContext): AvailableEntries = provingContext.availableEntries
  implicit def fromEntryParsingContext(implicit entryParsingContext: EntryParsingContext): AvailableEntries = entryParsingContext.availableEntries
}
