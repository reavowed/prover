package net.prover.model

case class Definition(connective: Connective, defininingStatement: Statement) extends TheoremLineParser {
  val definedStatement: Statement = ConnectiveStatement(
    (1 to connective.arity).map(Atom),
    connective)

  override val name: String = "definition-" + connective.name
  override def applyToTheorem(theoremBuilder: TheoremBuilder, text: String, book: Book): TheoremBuilder = {
    val (reference, _) = text.splitFirstWord
    val referredStatement = theoremBuilder.resolveReference(reference)
    val replacedStatement = definedStatement.attemptMatch(referredStatement).map(defininingStatement.replace)
        .orElse(defininingStatement.attemptMatch(referredStatement).map(definedStatement.replace))
        .getOrElse(throw new Exception(s"Could not apply definition to statement '$referredStatement'"))
    theoremBuilder.addStep(Step(replacedStatement))
  }
}

object Definition extends SingleLineBookEntryParser[Definition] {
  override val name: String = "definition"
  override def parse(definitionText: String, book: Book): Definition = {
    val (connectiveName, statementText) = definitionText.splitFirstWord
    val connective = book.connectives.find(_.name == connectiveName)
      .getOrElse(throw new Exception(s"Unrecognised connective '$connectiveName'"))
    val (definingStatement, _) = Statement.parse(statementText, book.connectives)
    Definition(connective, definingStatement)
  }
  override def addToBook(definition: Definition, book: Book): Book = {
    book.copy(definitions = book.definitions :+ definition)
  }
}
