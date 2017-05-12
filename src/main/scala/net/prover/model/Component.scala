package net.prover.model

trait Component {
  def componentType: ComponentType
  def allVariables: Variables
  def presentVariables: Variables
  def boundVariables: Set[TermVariable]
  def getPotentiallyIntersectingVariables(termVariable: TermVariable): Variables
  def calculateSubstitutions(other: Component, substitutions: PartialSubstitutions): Seq[PartialSubstitutions]
  def applySubstitutions(substitutions: Substitutions): Option[Component]
  def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[Component]
  def resolveSingleSubstitution(other: Component, termVariable: TermVariable, thisTerm: Term, otherTerm: Term): Option[(Component, DistinctVariables)]
  def validateSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    target: Component,
    distinctVariables: DistinctVariables
  ): Option[DistinctVariables]
  def findSubstitution(other: Component, termVariable: TermVariable): (Seq[(Term, DistinctVariables)], Option[DistinctVariables])
  def replacePlaceholder(other: Component): Option[Component]
  def html: String
  def safeHtml: String = html
  def serialized: String
  override def toString: String = html

  protected def findSubstitution(
    subcomponents: Seq[Component],
    otherSubcomponents: Seq[Component],
    termVariable: TermVariable
  ): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
    def combine(
      x: (Seq[(Term, DistinctVariables)], Option[DistinctVariables]),
      y: (Seq[(Term, DistinctVariables)], Option[DistinctVariables])
    ): (Seq[(Term, DistinctVariables)], Option[DistinctVariables]) = {
      val terms = (x._1.map(_._1) ++ y._1.map(_._1)).distinct
      val termSubstitutions = terms.map { term =>
        for {
          xSide <- x._1.find(_._1 == term).map(_._2).orElse(x._2)
          ySide <- y._1.find(_._1 == term).map(_._2).orElse(y._2)
        } yield (term, xSide ++ ySide)
      } collect {
        case Some(z) => z
      }
      val avoidingSubstitutions = for {
        xSide <- x._2
        ySide <- y._2
      } yield xSide ++ ySide
      (termSubstitutions, avoidingSubstitutions)
    }
    if (subcomponents.isEmpty) {
      (Nil, Some(DistinctVariables.empty))
    } else {
      subcomponents.zip(otherSubcomponents)
        .map {
          case (subcomponent, otherSubcomponent) =>
            subcomponent.findSubstitution(otherSubcomponent, termVariable)
        }
        .reduce(combine)
    }
  }

  protected def resolveSubstitution(
    subcomponents: Seq[Component],
    otherSubcomponents: Seq[Component],
    termVariable: TermVariable,
    thisTerm: Term,
    otherTerm: Term
  ): Option[(Seq[Component], DistinctVariables)] = {
    subcomponents.zip(otherSubcomponents)
      .map { case (subcomponent, otherSubcomponent) =>
        subcomponent.resolveSingleSubstitution(otherSubcomponent, termVariable, thisTerm, otherTerm)
      }
      .traverseOption
      .map(_.split)
      .map(_.mapRight(_.foldLeft(DistinctVariables.empty)(_ ++ _)))
  }

  protected def validateSubstitution(
    termToReplaceWith: Term,
    termToBeReplaced: TermVariable,
    subcomponents: Seq[Component],
    otherSubcomponents: Seq[Component],
    distinctVariables: DistinctVariables
  ): Option[DistinctVariables] = {
    subcomponents.zip(otherSubcomponents)
      .map { case (subcomponent, otherSubcomponent) =>
        subcomponent.validateSubstitution(termToReplaceWith, termToBeReplaced, otherSubcomponent, distinctVariables)
      }
      .traverseOption
      .map(_.foldLeft(DistinctVariables.empty)(_ ++ _))
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
  override def applySubstitutions(substitutions: Substitutions) = {
    throw new Exception("Cannot apply substitutions to placeholder")
  }
  override def makeSingleSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, distinctVariables: DistinctVariables): Option[T] = {
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
  override def validateSubstitution(termToReplaceWith: Term, termToBeReplaced: TermVariable, target: Component, distinctVariables: DistinctVariables) = {
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
