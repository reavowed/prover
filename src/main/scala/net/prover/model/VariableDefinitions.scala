package net.prover.model

object VariableDefinitions extends BookEntryParser {
  override def name: String = "variables"
  override def parser(book: Book): Parser[Book] = {
    for {
      statementVariableNames <- Parser.allInParens.map(_.splitByWhitespace())
      termVariableNames <- Parser.allInParens.map(_.splitByWhitespace())
    } yield {
      val statementVariables = statementVariableNames.map(StatementVariable).toSet
      val termVariables = termVariableNames.map(TermVariable).toSet
      val variables = Variables(statementVariables, termVariables)
      val updatedContext = book.context.copy(variables = book.context.variables ++ variables)
      val updatedBook = book.copy(context = updatedContext)
      updatedBook
    }
  }
}
