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

object Components {
  def listParser(componentTypes: Seq[ComponentType], context: Context):Parser[Seq[Component]] = {
    Parser { line =>
      componentTypes.foldLeft((Seq.empty[Component], line)) { case ((components, remainingLine), componentType) =>
        componentType.parse(remainingLine, context).mapLeft(components :+ _)
      }
    }.inParens
  }
}

trait ComponentType {
  def parse(line: PartialLine, context: Context): (Component, PartialLine)
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
}
