package net.prover.model

object VariableDefinitions extends BookEntryParser {
  override def name: String = "variables"
  override def parse(remainingLine: PartialLine, otherLines: Seq[BookLine], book: Book): (Book, Seq[BookLine]) = {
    val variables = Parser.allInParens
      .map(_.splitByWhitespace())
      .map(variableNames => Variables(Nil, variableNames.map(TermVariable)))
      .parse(remainingLine)._1
    val updatedContext = book.context.copy(variables = book.context.variables ++ variables)
    val updatedBook = book.copy(context = updatedContext)
    (updatedBook, otherLines)
  }
}
