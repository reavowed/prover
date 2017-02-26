package net.prover.model

object VariableDefinitions extends BookEntryParser {
  override def name: String = "variables"
  override def parser(book: Book, lines: Seq[BookLine]): Parser[(Book, Seq[BookLine])] = {
    val variablesParser = Parser.allInParens
      .map(_.splitByWhitespace())
      .map(variableNames => Variables(Nil, variableNames.map(TermVariable)))
    for {
      variables <- variablesParser
    } yield {
      val updatedContext = book.context.copy(variables = book.context.variables ++ variables)
      val updatedBook = book.copy(context = updatedContext)
      (updatedBook, lines)
    }
  }
}
