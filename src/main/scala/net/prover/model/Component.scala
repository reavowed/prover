package net.prover.model

trait Component {
  def componentType: ComponentType
  def allVariables: Variables
  def presentVariables: Variables = allVariables
  def boundVariables: Set[TermVariable]
  def calculateSubstitutions(other: Component, substitutions: PartialSubstitutions): Option[PartialSubstitutions]
  def applySubstitutions(substitutions: Substitutions): Component
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable): Component
  def resolveSingleSubstitution(other: Component, termVariable: TermVariable, thisTerm: Term, otherTerm: Term): Option[Component]
  def findSubstitution(other: Component, termVariable: TermVariable): Option[Option[Term]]
  def replacePlaceholder(other: Component): Component
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
          .getOrElse(throw new Exception(s"Unrecognised variable name '$variableName'"))
    }
  }
}

trait Placeholder extends Component {
  override def allVariables: Variables = Variables.empty
  override def boundVariables: Set[TermVariable] = Set.empty
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ): Nothing = {
    throw new Exception("Cannot calculate substitutions for placeholder")
  }
  override def applySubstitutions(substitutions: Substitutions): Nothing = {
    throw new Exception("Cannot apply substitutions to placeholder")
  }
  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable): Nothing = {
    throw new Exception("Cannot make substitution into placeholder")
  }
  def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Nothing = {
    throw new Exception("Cannot resolve substitution for placeholder")
  }
  def findSubstitution(other: Component, termVariable: TermVariable): Nothing = {

    throw new Exception("Cannot find substitution for placeholder")
  }
  override def html: String = "???"
  override def serialized: String = "_"
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
