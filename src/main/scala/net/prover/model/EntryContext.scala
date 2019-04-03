package net.prover.model

import net.prover.model.entries.{ChapterEntry, ExpressionDefinition, StatementDefinition, TermDefinition}
import net.prover.model.expressions.{DefinedExpression, DefinedStatement, ExpressionVariable, Statement}

case class EntryContext(availableEntries: Seq[ChapterEntry], termVariableNames: Seq[String]) {
  val inferences: Seq[Inference] = availableEntries.flatMap(_.inferences)
  val statementDefinitions: Seq[StatementDefinition] = availableEntries.ofType[StatementDefinition]
  val termDefinitions: Seq[TermDefinition] = availableEntries.ofType[TermDefinition]

  def addEntry(entry: ChapterEntry): EntryContext = copy(availableEntries = availableEntries :+ entry)
  def addEntries(entries: Seq[ChapterEntry]): EntryContext = copy(availableEntries = availableEntries ++ entries)

  def deductionStatementOption: Option[StatementDefinition] = {
    statementDefinitions.find(_.structureType.contains(StatementDefinition.StructureType.Deduction))
  }
  def scopingStatementOption: Option[StatementDefinition] = {
    statementDefinitions.find(_.structureType.contains(StatementDefinition.StructureType.Scoping))
  }

  def matchScopingStatement(statement: Statement): Option[(Statement, StatementDefinition)] = {
    scopingStatementOption.flatMap { scopingStatement =>
      statement match {
        case DefinedStatement(Seq(substatement), `scopingStatement`) =>
          substatement.asOptionalInstanceOf[Statement].map(_ -> scopingStatement)
        case _ =>
          None
      }
    }
  }
  def matchDeductionStatement(statement: Statement): Option[(Statement, Statement, StatementDefinition)] = {
    deductionStatementOption.flatMap { deductionStatement =>
      statement match {
        case DefinedStatement(Seq(antecedentExpression, consequentExpression), `deductionStatement`) =>
          for {
            antecedent <- antecedentExpression.asOptionalInstanceOf[Statement]
            consequent <- consequentExpression.asOptionalInstanceOf[Statement]
          } yield (antecedent, consequent, deductionStatement)
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
