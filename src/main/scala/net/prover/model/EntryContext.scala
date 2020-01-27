package net.prover.model

import net.prover.model.entries._
import net.prover.model.expressions._

case class EntryContext(availableEntries: Seq[ChapterEntry], termVariableNames: Seq[String]) extends EntryContext.EntryTypes {

  def addEntry(entry: ChapterEntry): EntryContext = copy(availableEntries = availableEntries :+ entry)
  def addEntries(entries: Seq[ChapterEntry]): EntryContext = copy(availableEntries = availableEntries ++ entries)

  lazy val conjunctionDefinitionOption: Option[StatementDefinition] = {
    statementDefinitions.find(_.attributes.contains("conjunction"))
  }

  object RecognisedStatementDefinition {
    def unapply(string: String): Option[StatementDefinition] = {
      statementDefinitions.find(_.symbol == string)
    }
  }
  object RecognisedTermDefinition {
    def unapply(s: String): Option[TermDefinition] = {
      termDefinitions.find(_.symbol == s)
    }
  }

  object RecognisedStatementShorthand {
    def unapply(string: String): Option[Template.DefinedStatement] = {
      writingShorthands.find(_.symbol == string).flatMap(_.template.asOptionalInstanceOf[Template.DefinedStatement])
    }
  }
  object RecognisedTermShorthand {
    def unapply(string: String): Option[Template.DefinedTerm] = {
      writingShorthands.find(_.symbol == string).flatMap(_.template.asOptionalInstanceOf[Template.DefinedTerm])
    }
  }
}

object EntryContext {
  trait EntryTypes {
    def availableEntries: Seq[ChapterEntry]
    lazy val inferences: Seq[Inference.FromEntry] = availableEntries.flatMap(_.inferences)
    lazy val statementDefinitions: Seq[StatementDefinition] = availableEntries.collect {
      case statementDefinition: StatementDefinition => statementDefinition
      case typeDefinition: TypeDefinition => typeDefinition.statementDefinition
      case propertyDefinition: PropertyDefinition => propertyDefinition.statementDefinition
    }
    lazy val termDefinitions: Seq[TermDefinition] = availableEntries.ofType[TermDefinition]
    lazy val typeDefinitions: Seq[TypeDefinition] = availableEntries.ofType[TypeDefinition]
    lazy val propertyDefinitionsByType: Map[String, Seq[PropertyDefinition]] = availableEntries.ofType[PropertyDefinition].groupBy(_.parentType.symbol)
    lazy val displayShorthands: Seq[DisplayShorthand] = availableEntries.ofType[DisplayShorthand]
    lazy val writingShorthands: Seq[WritingShorthand] = availableEntries.ofType[WritingShorthand]

    lazy val deductionDefinitionOption: Option[StatementDefinition] = {
      statementDefinitions.find(_.attributes.contains("deduction"))
    }
    lazy val scopingDefinitionOption: Option[StatementDefinition] = {
      statementDefinitions.find(_.attributes.contains("scoping"))
    }
  }

  def forBooks(books: Seq[Book], termVariableNames: Seq[String]): EntryContext = {
    EntryContext(books.flatMap(_.chapters).flatMap(_.entries), termVariableNames)
  }
  def forBookExclusive(allBooks: Seq[Book], book: Book): EntryContext = {
    forBooks(Book.getDependencies(book.imports, allBooks), book.termVariableNames)
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
