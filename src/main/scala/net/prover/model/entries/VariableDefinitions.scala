package net.prover.model.entries

import net.prover.model._

object VariableDefinitions extends BookEntryParser {
  override def name: String = "variables"
  override def parser(book: Book): Parser[Book] = {
    for {
      newStatementVariableNames <- Parser.allInParens.map(_.splitByWhitespace())
      newTermVariableNames <- Parser.allInParens.map(_.splitByWhitespace())
    } yield {
      val updatedContext = book.context.copy(
        statementVariableNames = book.context.statementVariableNames ++ newStatementVariableNames,
        termVariableNames = book.context.termVariableNames ++ newTermVariableNames)
      val updatedBook = book.copy(context = updatedContext)
      updatedBook
    }
  }
}
