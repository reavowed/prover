package net.prover.model

case class Definition(connective: Connective, defininingStatement: Statement)

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
