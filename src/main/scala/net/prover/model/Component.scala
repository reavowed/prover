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
  def parse(line: PartialLine): (ComponentType, PartialLine) = {
    line match {
      case WordAndRemainingText("term", lineAfterTerm) =>
        (Term, lineAfterTerm)
      case WordAndRemainingText("statement", lineAfterStatement) =>
        (Statement, lineAfterStatement)
      case WordAndRemainingText(something, _) =>
        throw ParseException.withMessage("Unrecognised statement type '$something'", line.fullLine)
    }
  }

  def parseList(line: PartialLine): (Seq[ComponentType], PartialLine) = {
    Parser.listInParens(line, parse, None)
  }

  def listParser: Parser[Seq[ComponentType]] = Parser(parseList)
}
