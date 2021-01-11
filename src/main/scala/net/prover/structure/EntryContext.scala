package net.prover.structure

import net.prover.model.definitions._
import net.prover.model.expressions._
import net.prover.model.template.{CompoundStatementTemplate, CompoundTermTemplate}
import net.prover.model.{Inference, Parser, ProvingContext}
import net.prover.structure.model.entries._
import net.prover.structure.model.{Book, Chapter}

case class EntryContext(availableEntries: Seq[ChapterEntry], inferencesById: Map[String, Inference], statementDefinitionsBySymbol: Map[String, CompoundStatementDefinition], termDefinitionsBySymbol: Map[String, CompoundTermDefinition]) {

  lazy val allInferences: Seq[Inference.FromEntry] = availableEntries.flatMap(_.inferences)
  lazy val allInferenceIds: Set[String] = inferencesById.keySet
  lazy val statementDefinitions: Seq[CompoundStatementDefinition] = availableEntries.mapCollect(EntryContext.getStatementDefinitionFromEntry)
  lazy val termDefinitions: Seq[CompoundTermDefinition] = availableEntries.ofType[CompoundTermDefinition]
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

  lazy val typeStatementDefinitionsByType: Map[String, Seq[CompoundStatementDefinition]] = {
    typeDefinitions.mapValues { t =>
      t.statementDefinition +: (qualifiersByType.getOrElse(t.symbol, Nil) ++ propertyDefinitionsByType.getOrElse(t.symbol, Nil) ++ relatedObjectsByType.getOrElse(t.symbol, Nil)).map(_.statementDefinition)
    }
  }
  lazy val typeStatementDefinitions: Seq[CompoundStatementDefinition] = typeStatementDefinitionsByType.values.flatten.toSeq ++ typeRelationDefinitions.map(_.statementDefinition)

  def addEntry(entry: ChapterEntry): EntryContext = {
    addEntries(Seq(entry))
  }
  def addEntries(entries: Seq[ChapterEntry]): EntryContext = {
    this ++ EntryContext(entries)
  }


  object RecognisedCompoundStatementDefinition {
    def unapply(symbol: String): Option[CompoundStatementDefinition] = {
      statementDefinitionsBySymbol.get(symbol)
    }
  }
  object RecognisedCompoundTermDefinition {
    def unapply(symbol: String): Option[CompoundTermDefinition] = {
      termDefinitionsBySymbol.get(symbol)
    }
  }

  object RecognisedStatementShorthand {
    def unapply(string: String): Option[CompoundStatementTemplate] = {
      writingShorthands.find(_.symbol == string).flatMap(_.template.asOptionalInstanceOf[CompoundStatementTemplate])
    }
  }
  object RecognisedTermShorthand {
    def unapply(string: String): Option[CompoundTermTemplate] = {
      writingShorthands.find(_.symbol == string).flatMap(_.template.asOptionalInstanceOf[CompoundTermTemplate])
    }
  }

  def typeDefinitionParser: Parser[TypeDefinition] = Parser.singleWord.map(typeName => typeDefinitions.getOrElse(typeName, throw new Exception(s"Unrecognised type '$typeName'")))

  def ++ (other: EntryContext) = {
    EntryContext(
      availableEntries ++ other.availableEntries,
      inferencesById ++ other.inferencesById,
      statementDefinitionsBySymbol ++ other.statementDefinitionsBySymbol,
      termDefinitionsBySymbol ++ other.termDefinitionsBySymbol)
  }
}

object EntryContext {

  def getStatementDefinitionFromEntry(entry: ChapterEntry): Option[CompoundStatementDefinition] = entry match {
    case statementDefinition: CompoundStatementDefinition =>
      Some(statementDefinition)
    case entryWithStatementDefinition: ChapterEntry.HasStatementDefinition =>
      Some(entryWithStatementDefinition.statementDefinition)
    case _ =>
      None
  }

  def apply(entries: Seq[ChapterEntry]): EntryContext = {
    EntryContext(
      entries,
      entries.flatMap(_.inferences).toMapWithKey(_.id),
      entries.mapCollect(getStatementDefinitionFromEntry).toMapWithKey(_.symbol),
      entries.ofType[CompoundTermDefinition].toMapWithKey(_.disambiguatedSymbol.serialized))
  }


  def forBooks(books: Seq[Book]): EntryContext = {
    EntryContext(books.flatMap(_.chapters).flatMap(_.entries))
  }
  def forBookExclusive(allBooks: Seq[Book], book: Book): EntryContext = {
    forBooks(Book.getDependencies(book.imports, allBooks))
  }
  def forChapterExclusive(allBooks: Seq[Book], book: Book, chapter: Chapter): EntryContext = {
    forBookExclusive(allBooks, book).addEntries(book.chapters.until(chapter).flatMap(_.entries))
  }
  def forChapterInclusive(allBooks: Seq[Book], book: Book, chapter: Chapter): EntryContext = {
    forChapterExclusive(allBooks, book, chapter).addEntries(chapter.entries)
  }
  def forEntry(allBooks: Seq[Book], book: Book, chapter: Chapter, entry: ChapterEntry): EntryContext = {
    forChapterExclusive(allBooks, book, chapter).addEntries(chapter.entries.until(entry))
  }

  implicit def fromProvingContext(implicit provingContext: ProvingContext): EntryContext = provingContext.entryContext
}
