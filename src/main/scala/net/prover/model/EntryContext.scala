package net.prover.model

import net.prover.model.Inference.RearrangementType
import net.prover.model.entries._
import net.prover.model.expressions._

case class EntryContext(availableEntries: Seq[ChapterEntry], termVariableNames: Seq[String]) {
  val inferences: Seq[Inference] = availableEntries.flatMap(_.inferences)
  val statementDefinitions: Seq[StatementDefinition] = availableEntries.collect {
    case statementDefinition: StatementDefinition => statementDefinition
    case typeDefinition: TypeDefinition => typeDefinition.statementDefinition
    case propertyDefinition: PropertyDefinition => propertyDefinition.statementDefinition
  }
  val termDefinitions: Seq[TermDefinition] = availableEntries.ofType[TermDefinition]
  val typeDefinitions: Seq[TypeDefinition] = availableEntries.ofType[TypeDefinition]
  val propertyDefinitionsByType: Map[String, Seq[PropertyDefinition]] = availableEntries.ofType[PropertyDefinition].groupBy(_.parentType.symbol)
  val writingShorthands: Seq[WritingShorthand] = availableEntries.ofType[WritingShorthand]

  lazy val simplificationInferences: Seq[Inference] = inferences.filter(_.rearrangementType == RearrangementType.Simplification)

  def addEntry(entry: ChapterEntry): EntryContext = copy(availableEntries = availableEntries :+ entry)
  def addEntries(entries: Seq[ChapterEntry]): EntryContext = copy(availableEntries = availableEntries ++ entries)

  def deductionDefinitionOption: Option[StatementDefinition] = {
    statementDefinitions.find(_.attributes.contains("deduction"))
  }
  def scopingDefinitionOption: Option[StatementDefinition] = {
    statementDefinitions.find(_.attributes.contains("scoping"))
  }
  def conjunctionDefinitionOption: Option[StatementDefinition] = {
    statementDefinitions.find(_.attributes.contains("conjunction"))
  }

  def matchScopingStatement(statement: Statement): Option[(Statement, StatementDefinition)] = {
    scopingDefinitionOption.flatMap { scopingDefinition =>
      statement match {
        case DefinedStatement(Seq(substatement), `scopingDefinition`) =>
          substatement.asOptionalInstanceOf[Statement].map(_ -> scopingDefinition)
        case _ =>
          None
      }
    }
  }
  def matchDeductionStatement(statement: Statement): Option[(Statement, Statement, StatementDefinition)] = {
    deductionDefinitionOption.flatMap { deductionDefinition =>
      statement match {
        case DefinedStatement(Seq(antecedentExpression, consequentExpression), `deductionDefinition`) =>
          for {
            antecedent <- antecedentExpression.asOptionalInstanceOf[Statement]
            consequent <- consequentExpression.asOptionalInstanceOf[Statement]
          } yield (antecedent, consequent, deductionDefinition)
        case _ =>
          None
      }
    }
  }

  def transitivityInferences: Map[ExpressionDefinition, Inference] = {
    inferences.mapCollect {
      case inference @ Inference(
        _,
        Seq(
          DefinedExpression(d1, Nil, Seq(ExpressionVariable(a1), ExpressionVariable(b1))),
          DefinedExpression(d2, Nil, Seq(ExpressionVariable(b2), ExpressionVariable(c1)))),
        DefinedExpression(d3, Nil, Seq(ExpressionVariable(a2), ExpressionVariable(c2)))
      ) if d1 == d2 && d2 == d3 && a1 == a2 && b1 == b2 && c1 == c2 =>
        Some((d1, inference))
      case _ =>
        None
    }.toMap
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
}
