package net.prover.model

import net.prover.model.components._

case class Substitutions(
    componentsByVariable: Map[Variable, Component],
    distinctVariables: DistinctVariables)
{
  def serialized: String = {
    val serializedComponentsByVariable = componentsByVariable.toSeq
      .sortBy { case (variable, component) => variable.text }
      .map { case (variable, component) => s"${variable.serialized} -> ${component.serialized}"}
      .mkString(", ")
    val serializedDistinctVariables = distinctVariables.serialized
    s"($serializedComponentsByVariable) ($serializedDistinctVariables)"
  }
}

object Substitutions {
  def parser(implicit parsingContext: ParsingContext): Parser[Substitutions] = {
    for {
      componentsByVariable <- pairParser.listInParens(Some(",")).map(_.toMap)
      distinctVariables <- DistinctVariables.parser
    } yield {
      Substitutions(componentsByVariable, distinctVariables)
    }
  }
  private def pairParser(implicit parsingContext: ParsingContext): Parser[(Variable, Component)] = {
    for {
      variable <- Variable.parser
      _ <- Parser.requiredWord("->")
      component <- Component.parser
    } yield (variable, component)
  }
}

case class PartialSubstitutions(
    known: Map[Variable, Component],
    unknown: Map[SubstitutedVariable[Component, _ <: Variable], Component],
    distinctVariables: DistinctVariables) {

  def knownSubstitutions = Substitutions(known, distinctVariables)

  def tryAdd(variable: Variable, component: Component): Option[PartialSubstitutions] = {
    known.get(variable) match {
      case Some(`component`) =>
        Some(this)
      case Some(_) =>
        None
      case None =>
        Some(copy(known = known + (variable -> component)))
    }
  }

  def tryAdd(
    substitutedVariable: SubstitutedVariable[Component, _ <: Variable],
    component: Component
  ): Seq[PartialSubstitutions] = {
    val directAttempts = tryAddingDirectly(substitutedVariable, component)
    if (directAttempts.nonEmpty)
      directAttempts
    else {
      findMergableSubstitution(substitutedVariable).toSeq.flatMap(tryAddingPotentialSubstitutions(substitutedVariable, _, component)) :+
        copy(unknown = unknown + (substitutedVariable -> component))
    }
  }

  private def tryAddingDirectly(
    substitutedVariable: SubstitutedVariable[Component, _ <: Variable],
    target: Component
  ): Seq[PartialSubstitutions] = {
    val substitutedTailOption = substitutedVariable.tail.applySubstitutions(knownSubstitutions)
    val substitutedTermToReplaceWithOption = substitutedVariable.firstTerm.applySubstitutions(knownSubstitutions)
    val substitutedTermToBeReplacedOption = known.get(substitutedVariable.firstVariable).map(_.asInstanceOf[Term]).flatMap(Term.optionAsVariable)

    (substitutedTailOption, substitutedTermToReplaceWithOption, substitutedTermToBeReplacedOption) match {
      case (Some(substitutedTail), Some(substitutedTermToReplaceWith), Some(substitutedTermToBeReplaced)) =>
        tryNotAddingAtAll(substitutedTail, substitutedTermToReplaceWith, substitutedTermToBeReplaced, target)
      case (Some(substitutedTail), None, Some(substitutedTermToBeReplaced)) =>
        tryAddingByCalculatingTermToReplaceWith(
          substitutedTail,
          substitutedTermToBeReplaced,
          substitutedVariable.firstTerm,
          target)
      case _ =>
        findMergableSubstitution(substitutedVariable) match {
          case Some(otherSubstitutedVariable) =>
            tryAddingByMerge(substitutedVariable, otherSubstitutedVariable, target).toSeq
          case _ =>
            Nil
        }
    }
  }

  private def tryNotAddingAtAll(
    substitutedBaseComponent: Component,
    substitutedTermToReplaceWith: Term,
    substitutedTermToBeReplaced: TermVariable,
    targetComponent: Component
  ): Seq[PartialSubstitutions] = {
    substitutedBaseComponent
      .validateSingleSubstitution(substitutedTermToReplaceWith, substitutedTermToBeReplaced, targetComponent, distinctVariables)
      .toSeq
      .map(withDistinctVariables)
  }

  private def tryAddingByCalculatingTermToReplaceWith(
    substitutedBaseComponent: Component,
    substitutedTermToBeReplaced: TermVariable,
    termToReplaceWith: Term,
    targetComponent: Component
  ): Seq[PartialSubstitutions] = {
    for {
      (substitutedTermToReplaceWith, newDistinctVariables) <- substitutedBaseComponent
        .findSubstitution(targetComponent, substitutedTermToBeReplaced)._1
      updatedSubstitutions <- termToReplaceWith.calculateSubstitutions(substitutedTermToReplaceWith, this)
      additionalDistinctVariables = getAdditionalDistinctVariableConditionsForTermReplacement(
        substitutedBaseComponent,
        substitutedTermToBeReplaced,
        substitutedTermToReplaceWith)
    } yield {
      updatedSubstitutions.withDistinctVariables(newDistinctVariables ++ additionalDistinctVariables)
    }
  }

  private def getAdditionalDistinctVariableConditionsForTermReplacement(
    substitutedBaseComponent: Component,
    substitutedTermToBeReplaced: TermVariable,
    substitutedTermToReplaceWith: Term
  ): DistinctVariables = {
    if (substitutedTermToBeReplaced == substitutedTermToReplaceWith)
        DistinctVariables.empty
    else {
      val variablesRequiringCondition = (substitutedBaseComponent.presentVariables.ofType[TermVariable] - substitutedTermToBeReplaced) --
        Term.optionAsVariable(substitutedTermToReplaceWith).toSet
      variablesRequiringCondition
        .foldLeft(DistinctVariables.empty) { case (variables, variable) =>
          variables + (substitutedTermToBeReplaced, variable)
        }
    }
  }

  private def findMergableSubstitution(
    substitutedVariable: SubstitutedVariable[Component, _ <: Variable]
  ): Option[SubstitutedVariable[Component, _ <: Variable]] = {
    unknown.keySet
      .find { s =>
        s.tail == substitutedVariable.tail &&
          s.firstVariable == substitutedVariable.firstVariable
      }
  }

  private def tryAddingByMerge(
    newSubstitutedVariable: SubstitutedVariable[Component, _ <: Variable],
    otherSubstitutedVariable: SubstitutedVariable[Component, _ <: Variable],
    thisTarget: Component
  ): Option[PartialSubstitutions] = {
    val otherTarget = unknown(otherSubstitutedVariable)
    val sharedTermToBeReplaced = otherSubstitutedVariable.firstVariable
    for {
      thisTermToReplaceWith <- newSubstitutedVariable.firstTerm.applySubstitutions(knownSubstitutions)
      otherTermToReplaceWith <- otherSubstitutedVariable.firstTerm.applySubstitutions(knownSubstitutions)
      placeholderVariableTerm = known.getOrElse(
        sharedTermToBeReplaced,
        sharedTermToBeReplaced.copy(text = sharedTermToBeReplaced + "'")
      ).asInstanceOf[Term]
      placeholderVariable <- Term.optionAsVariable(placeholderVariableTerm)
      (resolvedStatement, additionalDistinctVariables) <- thisTarget.resolveSingleSubstitution(
        otherTarget,
        placeholderVariable,
        thisTermToReplaceWith,
        otherTermToReplaceWith)
      substitutionsWithoutOtherSubstitutedStatementVariable =
        copy(unknown = unknown - otherSubstitutedVariable)
      substitutionsWithResolvedStatement <- substitutionsWithoutOtherSubstitutedStatementVariable
        .tryAdd(newSubstitutedVariable.variable, resolvedStatement)
      substitutionsWithSharedTermToBeReplaced <- substitutionsWithResolvedStatement
        .tryAdd(sharedTermToBeReplaced, placeholderVariable)
    } yield substitutionsWithSharedTermToBeReplaced.withDistinctVariables(additionalDistinctVariables)
  }

  private def tryAddingPotentialSubstitutions(
    newSubstitutedVariable: SubstitutedVariable[Component, _ <: Variable],
    otherSubstitutedVariable: SubstitutedVariable[Component, _ <: Variable],
    thisTarget: Component
  ): Seq[PartialSubstitutions] = {
    val otherTarget = unknown(otherSubstitutedVariable)
    for {
      (a, b) <- thisTarget.getDifferences(otherTarget).toSeq
      s1 <- newSubstitutedVariable.firstTerm.calculateSubstitutions(a, this)
      s2 <- otherSubstitutedVariable.firstTerm.calculateSubstitutions(b, s1)
    } yield s2.copy(unknown = s2.unknown + (newSubstitutedVariable -> thisTarget))
  }

  def withDistinctVariables(additionalDistinctVariables: DistinctVariables): PartialSubstitutions = {
    copy(distinctVariables = distinctVariables ++ additionalDistinctVariables)
  }

  def tryResolve(): Seq[Substitutions] = {
    unknown.keys.toList match {
      case Nil =>
        Seq(knownSubstitutions)
      case one +: more =>
        copy(unknown = unknown.filterKeys(more.contains))
          .tryAddingDirectly(one, unknown(one))
          .flatMap(_.tryResolve())
    }
  }
}

object PartialSubstitutions {
  val empty: PartialSubstitutions = PartialSubstitutions(Map.empty, Map.empty, DistinctVariables.empty)
}
