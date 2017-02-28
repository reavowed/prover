package net.prover.model

trait Component {
  def variables: Variables
  def freeVariables: Seq[TermVariable]
  def calculateSubstitutions(other: Component): Option[Substitutions]
  def applySubstitutions(substitutions: Substitutions): Component
  def substituteFreeVariable(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable
  ): Component
  def attemptSimplification(other: Component): Option[DistinctVariables]
  def makeSimplifications(distinctVariables: DistinctVariables): Component
  def html: String
  def safeHtml: String = html
  override def toString: String = html
}

trait ComponentType {
  def parser(context: Context): Parser[Component]
}

object ComponentType {

  private val componentTypesByName = Map(
    "term" -> Term,
    "statement" -> Statement)

  def parser: Parser[ComponentType] = {
    Parser.singleWord.mapWithLine { (name, line) =>
      componentTypesByName.getOrElse(name, line.throwParseException(s"Unrecognised statement type $name"))
    }
  }

  def listParser: Parser[Seq[ComponentType]] = parser.listInParens(None)

  implicit class ComponentTypeSeqOps(componentTypes: Seq[ComponentType]) {
    def componentsParser(context: Context) = {
      componentTypes.map(_.parser(context)).traverseParser
    }
  }
}
