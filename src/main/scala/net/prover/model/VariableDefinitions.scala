package net.prover.model

object VariableDefinitions extends BookEntryParser {
  override def name: String = "variables"
  override def parser(book: Book, lines: Seq[BookLine]): Parser[(Book, Seq[BookLine])] = {
    for {
      variableNames <- Parser.allInParens.map(_.splitByWhitespace())
    } yield {
      val variables = Variables(Nil, variableNames.map(TermVariable))
      val updatedContext = book.context.copy(variables = book.context.variables ++ variables)
      val updatedBook = book.copy(context = updatedContext)
      (updatedBook, lines)
    }
  }
}
