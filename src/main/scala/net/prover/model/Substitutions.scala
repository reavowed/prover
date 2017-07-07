package net.prover.model

case class Substitutions(
    componentsByVariable: Map[Variable, Component],
    distinctVariables: DistinctVariables)

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
    val substitutedBaseComponentOption = known.get(substitutedVariable.variable)
    val substitutedTermToReplaceWithOption = substitutedVariable.termToReplaceWith.applySubstitutions(knownSubstitutions)
    val substitutedTermToBeReplacedOption = known.get(substitutedVariable.termToBeReplaced).map(_.asInstanceOf[Term]).flatMap(Term.optionAsVariable)

    (substitutedBaseComponentOption, substitutedTermToReplaceWithOption, substitutedTermToBeReplacedOption) match {
      case (Some(substitutedBaseComponent), Some(substitutedTermToReplaceWith), Some(substitutedTermToBeReplaced)) =>
        tryNotAddingAtAll(substitutedBaseComponent, substitutedTermToReplaceWith, substitutedTermToBeReplaced, target)
      case (Some(substitutedBaseComponent), None, Some(substitutedTermToBeReplaced)) =>
        tryAddingByCalculatingTermToReplaceWith(
          substitutedBaseComponent,
          substitutedTermToBeReplaced,
          substitutedVariable.termToReplaceWith,
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
        s.variable == substitutedVariable.variable &&
          s.termToBeReplaced == substitutedVariable.termToBeReplaced
      }
  }

  private def tryAddingByMerge(
    newSubstitutedVariable: SubstitutedVariable[Component, _ <: Variable],
    otherSubstitutedVariable: SubstitutedVariable[Component, _ <: Variable],
    thisTarget: Component
  ): Option[PartialSubstitutions] = {
    val otherTarget = unknown(otherSubstitutedVariable)
    val sharedTermToBeReplaced = otherSubstitutedVariable.termToBeReplaced
    for {
      thisTermToReplaceWith <- newSubstitutedVariable.termToReplaceWith.applySubstitutions(knownSubstitutions)
      otherTermToReplaceWith <- otherSubstitutedVariable.termToReplaceWith.applySubstitutions(knownSubstitutions)
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
      s1 <- newSubstitutedVariable.termToReplaceWith.calculateSubstitutions(a, this)
      s2 <- otherSubstitutedVariable.termToReplaceWith.calculateSubstitutions(b, s1)
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
