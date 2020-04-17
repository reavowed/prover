package net.prover.model

import net.prover.model.entries._
import net.prover.model.expressions._

case class EntryContext(availableEntries: Seq[ChapterEntry], inferencesById: Map[String, Inference], statementDefinitionsBySymbol: Map[String, StatementDefinition], termDefinitionsBySymbol: Map[String, TermDefinition]) {

  lazy val allInferences: Seq[Inference.FromEntry] = availableEntries.flatMap(_.inferences)
  lazy val statementDefinitions: Seq[StatementDefinition] = availableEntries.mapCollect(EntryContext.getStatementDefinitionFromEntry)
  lazy val termDefinitions: Seq[TermDefinition] = availableEntries.ofType[TermDefinition]
  lazy val typeDefinitions: Seq[TypeDefinition] = availableEntries.ofType[TypeDefinition]
  lazy val propertyDefinitionsByType: Map[String, Seq[PropertyDefinitionOnType]] = availableEntries.ofType[PropertyDefinitionOnType].groupBy(_.parentType.symbol)
  lazy val standalonePropertyDefinitions: Seq[StandalonePropertyDefinition] = availableEntries.ofType[StandalonePropertyDefinition]
  lazy val displayShorthands: Seq[DisplayShorthand] = availableEntries.ofType[DisplayShorthand]
  lazy val writingShorthands: Seq[WritingShorthand] = availableEntries.ofType[WritingShorthand]

  lazy val deductionDefinitionOption: Option[StatementDefinition] = {
    statementDefinitions.find(_.attributes.contains("deduction"))
  }
  lazy val generalizationDefinitionOption: Option[StatementDefinition] = {
    statementDefinitions.find(_.attributes.contains("generalization"))
  }

  def addEntry(entry: ChapterEntry): EntryContext = {
    addEntries(Seq(entry))
  }
  def addEntries(entries: Seq[ChapterEntry]): EntryContext = {
    this ++ EntryContext(entries)
  }

  lazy val conjunctionDefinitionOption: Option[StatementDefinition] = {
    statementDefinitions.find(_.attributes.contains("conjunction"))
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

  def ++ (other: EntryContext) = {
    EntryContext(
      availableEntries ++ other.availableEntries,
      inferencesById ++ other.inferencesById,
      statementDefinitionsBySymbol ++ other.statementDefinitionsBySymbol,
      termDefinitionsBySymbol ++ other.termDefinitionsBySymbol)
  }
}

object EntryContext {

  def getStatementDefinitionFromEntry(entry: ChapterEntry): Option[StatementDefinition] = entry match {
    case statementDefinition: StatementDefinition => Some(statementDefinition)
    case typeDefinition: TypeDefinition => Some(typeDefinition.statementDefinition)
    case propertyDefinition: PropertyDefinitionOnType => Some(propertyDefinition.newStatementDefinition)
    case standalonePropertyDefinition: StandalonePropertyDefinition => Some(standalonePropertyDefinition.statementDefinition)
    case _ => None
  }

  def apply(entries: Seq[ChapterEntry]): EntryContext = {
    EntryContext(
      entries,
      entries.flatMap(_.inferences).toMapWithKey(_.id),
      entries.mapCollect(getStatementDefinitionFromEntry).toMapWithKey(_.symbol),
      entries.ofType[TermDefinition].toMapWithKey(_.disambiguatedSymbol.serialized))
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
