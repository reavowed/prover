package net.prover.model

trait Component {
  def componentType: ComponentType
  def allVariables: Variables
  def presentVariables: Variables
  def boundVariables: Set[TermVariable]
  def getPotentiallyIntersectingVariables(termVariable: TermVariable): Variables
  def calculateSubstitutions(other: Component, substitutions: PartialSubstitutions): Seq[PartialSubstitutions]
  def applySubstitutions(substitutions: Substitutions, distinctVariables: Map[TermVariable, Variables]): Option[Component]
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: Map[TermVariable, Variables]): Option[Component]
  def resolveSingleSubstitution(other: Component, termVariable: TermVariable, thisTerm: Term, otherTerm: Term): Option[(Component, Map[TermVariable, Variables])]
  def validateSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    target: Component,
    distinctVariables: Map[TermVariable, Variables]
  ): Option[Map[TermVariable, Variables]]
  def findSubstitution(other: Component, termVariable: TermVariable): Seq[(Option[Term], Map[TermVariable, Variables])]
  def replacePlaceholder(other: Component): Option[Component]
  def html: String
  def safeHtml: String = html
  def serialized: String
  override def toString: String = html

  protected def findSubstitution(
    subcomponents: Seq[Component],
    otherSubcomponents: Seq[Component],
    termVariable: TermVariable
  ): Seq[(Option[Term], Map[TermVariable, Variables])] = {
    def combine(
      x: (Option[Term], Map[TermVariable, Variables]),
      y: (Option[Term], Map[TermVariable, Variables])
    ): Option[(Option[Term], Map[TermVariable, Variables])] = {
      x._1 match {
        case Some(t) =>
          if (y._1.exists(_ != t))
            None
          else
            Some(Some(t), x._2 merge y._2)
        case None =>
          Some(y._1, x._2 merge y._2)
      }
    }
    if (subcomponents.isEmpty) {
      Seq((None, Map.empty))
    } else {
      val a: Seq[Seq[(Option[Term], Map[TermVariable, Variables])]] = subcomponents.zip(otherSubcomponents)
        .map {
          case (subcomponent, otherSubcomponent) =>
            subcomponent.findSubstitution(otherSubcomponent, termVariable)
        }
      val b = a
        .reduce[Seq[(Option[Term], Map[TermVariable, Variables])]] { case (acc, more) =>
          for {
            x <- acc
            y <- more
            z <- combine(x, y)
          } yield z
        }
      b
    }
  }

  protected def resolveSubstitution(
    subcomponents: Seq[Component],
    otherSubcomponents: Seq[Component],
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[(Seq[Component], Map[TermVariable, Variables])] = {
    subcomponents.zip(otherSubcomponents)
      .map { case (subcomponent, otherSubcomponent) =>
        subcomponent.resolveSingleSubstitution(otherSubcomponent, termVariable, thisTerm, otherTerm)
      }
      .traverseOption
      .map(_.split)
      .map(_.mapRight(_.foldLeft(Map.empty[TermVariable, Variables])(_ merge _)))
  }

  protected def validateSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    subcomponents: Seq[Component],
    otherSubcomponents: Seq[Component],
    distinctVariables: Map[TermVariable, Variables]
  ): Option[Map[TermVariable, Variables]] = {
    subcomponents.zip(otherSubcomponents)
      .map { case (subcomponent, otherSubcomponent) =>
        subcomponent.validateSubstitution(termToReplaceWith, termToBeReplaced, otherSubcomponent, distinctVariables)
      }
      .traverseOption
      .map(_.foldLeft(Map.empty[TermVariable, Variables])(_ merge _))
  }
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

trait Placeholder[T <: Component] extends Component {
  override def allVariables: Variables = Variables.empty
  override def presentVariables: Variables = Variables.empty
  override def boundVariables: Set[TermVariable] = Set.empty
  def getPotentiallyIntersectingVariables(termVariable: TermVariable): Variables = Variables.empty
  override def calculateSubstitutions(
    other: Component,
    substitutions: PartialSubstitutions
  ) = {
    throw new Exception("Cannot calculate substitutions for placeholder")
  }
  override def applySubstitutions(substitutions: Substitutions, distinctVariables: Map[TermVariable, Variables]) = {
    throw new Exception("Cannot apply substitutions to placeholder")
  }
  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: Map[TermVariable, Variables]): Option[T] = {
    throw new Exception("Cannot make substitution into placeholder")
  }
  def resolveSingleSubstitution(
    other: Component,
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ) = {
    throw new Exception("Cannot resolve substitution for placeholder")
  }
  override def validateSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, target: Component, distinctVariables: Map[TermVariable, Variables]) = {
    throw new Exception("Cannot validate substitution for placeholder")
  }
  def findSubstitution(other: Component, termVariable: TermVariable) = {

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
