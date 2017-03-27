package net.prover.model

trait Component {
  def componentType: ComponentType
  def variables: Variables
  def allBoundVariables: Seq[TermVariable]
  def calculateSubstitutions(other: Component, substitutions: Substitutions): Option[Substitutions]
  def applySubstitutions(substitutions: Substitutions): Component
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable): Component
  def html: String
  def safeHtml: String = html
  def serialized: String
  override def toString: String = html
}

object Component {
  def variableParser(implicit context: Context): Parser[Component] = {
    for {
      variableName <- Parser.singleWord
    } yield {
      context.variables.statementVariables.find(_.text == variableName)
          .orElse(context.variables.termVariables.find(_.text == variableName))
          .getOrElse(throw new Exception(s"Unrecognised variable name '$variableName"))
    }
  }
}

trait ComponentType {
  def parser(implicit context: Context): Parser[Component]
}

object ComponentType {
  private val componentTypesByName = Map(
    "term" -> Term,
    "statement" -> Statement)

  def parser: Parser[ComponentType] = {
    for {
      name <- Parser.singleWord
    } yield {
      componentTypesByName.getOrElse(
        name,
        throw new Exception(s"Unrecognised statement type $name"))
    }
  }

  def listParser: Parser[Seq[ComponentType]] = parser.listInParens(None)

  implicit class ComponentTypeSeqOps(componentTypes: Seq[ComponentType]) {
    def componentsParser(implicit context: Context) = {
      componentTypes.map(_.parser).traverseParser
    }
  }
}
