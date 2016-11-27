package net.prover.model

case class Definition(connective: Connective, definingStatement: Statement) extends ChapterEntry with TheoremLineParser {
  val `type` = "definition"
  val definedStatement: Statement = ConnectiveStatement(
    (1 to connective.arity).map(Atom),
    connective)

  override val name: String = "definition-" + connective.name
  override def applyToTheorem(theoremBuilder: TheoremBuilder, line: PartialLine, book: Book): TheoremBuilder = {
    val (reference, _) = line.splitFirstWord
    val referredStatement = theoremBuilder.resolveReference(reference)
    val replacedStatement = definedStatement.attemptMatch(referredStatement).map(definingStatement.replace)
        .orElse(definingStatement.attemptMatch(referredStatement).map(definedStatement.replace))
        .getOrElse(throw new Exception(s"Could not apply definition to statement '$referredStatement'"))
    theoremBuilder.addStep(Step(replacedStatement))
  }
}

object Definition extends SingleLineChapterEntryParser[Definition] {
  override val name: String = "definition"
  override def parse(line: PartialLine, book: Book): Definition = {
    val (connectiveName, lineAfterConnectiveName) = line.splitFirstWord
    val connective = book.connectives.find(_.name == connectiveName)
      .getOrElse(throw ParseException.withMessage(s"Unrecognised connective '$connectiveName'", line.fullLine))
    val (definingStatement, _) = Statement.parse(lineAfterConnectiveName, book.connectives)
    Definition(connective, definingStatement)
  }
  override def addToBook(definition: Definition, book: Book): Book = {
    book.copy(definitions = book.definitions :+ definition)
  }
}
