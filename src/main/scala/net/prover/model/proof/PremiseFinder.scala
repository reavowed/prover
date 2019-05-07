package net.prover.model.proof

import net.prover.model.entries.StatementDefinition
import net.prover.model.{EntryContext, Inference, Substitutions}
import net.prover.model.expressions.{DefinedStatement, Expression, Statement, Term}

import scala.util.Try

object PremiseFinder {

  def findParameterisedPremiseSteps(
    targetStatement: Statement,
    terms: Map[Int, Term],
    entryContext: EntryContext,
    premiseContext: PremiseContext,
    stepContext: StepContext
  ): Seq[(Seq[Step], Map[Int, Term])] = {
    def fromGivenPremises = premiseContext.allPremisesSimplestFirst
      .map(_.statement)
      .mapCollect { premiseStatement =>
        for {
          terms <- targetStatement.calculateArguments(premiseStatement, terms, 0, stepContext.externalDepth)
        } yield (Nil, terms)
      }

    def asAlreadyKnown = for {
      knownTarget <- targetStatement.specify(terms, 0, stepContext.externalDepth)
      steps <- findPremiseSteps(knownTarget, entryContext, premiseContext, stepContext)
    } yield (steps, terms)

    asAlreadyKnown.map(Seq(_)) getOrElse fromGivenPremises
  }
  def findParameterisedPremiseSteps(
    targetStatements: Seq[Statement],
    initialTerms: Map[Int, Term],
    entryContext: EntryContext,
    premiseContext: PremiseContext,
    stepContext: StepContext
  ): Seq[(Seq[Step], Map[Int, Term])] = {
    targetStatements.foldLeft(Seq((Seq.empty[Step], initialTerms))) { case (stepsAndTermsSoFar, targetStatement) =>
      for {
        (currentSteps, currentTerms) <- stepsAndTermsSoFar
        (newSteps, newTerms) <- findParameterisedPremiseSteps(targetStatement, currentTerms, entryContext, premiseContext, stepContext)
      } yield (currentSteps ++ newSteps, newTerms)
    }
  }

  def findPremiseSteps(
    targetStatement: Statement,
    entryContext: EntryContext,
    premiseContext: PremiseContext,
    stepContext: StepContext
  ): Option[Seq[Step]] = {
    val premiseSimplificationInferences = entryContext.availableEntries.ofType[Inference].collect {
      case inference @ Inference(_, Seq(singlePremise), conclusion)
        if singlePremise.complexity > conclusion.complexity &&
          singlePremise.requiredSubstitutions.isEquivalentTo(inference.requiredSubstitutions) &&
          singlePremise.requiredSubstitutions.predicates.isEmpty && singlePremise.requiredSubstitutions.functions.isEmpty &&
          conclusion.referencedDefinitions.subsetOf(singlePremise.referencedDefinitions)
      =>
        (inference, singlePremise)
    }
    val targetSimplificationInferences = entryContext.availableEntries.ofType[Inference].filter {
      case inference @ Inference(_, premises, conclusion)
        if premises.nonEmpty &&
          premises.forall(_.complexity < conclusion.complexity) &&
          conclusion.requiredSubstitutions.isEquivalentTo(inference.requiredSubstitutions) &&
          conclusion.requiredSubstitutions.predicates.isEmpty && conclusion.requiredSubstitutions.functions.isEmpty &&
          premises.forall(_.referencedDefinitions.subsetOf(conclusion.referencedDefinitions))
      =>
        true
      case _ =>
        false
    }

    def fromGivenPremises = premiseContext.allPremisesSimplestFirst
      .map(_.statement)
      .find(_ == targetStatement)
      .map(_ => Nil)
    def fromFact = ProofHelper.findFact(targetStatement, stepContext, entryContext).map(Seq(_))

    def bySimplifyingPremise(givenPremise: Statement): Option[Seq[Step]] = {
      if (givenPremise == targetStatement)
        Some(Nil)
      else if (givenPremise.complexity > targetStatement.complexity && givenPremise.definitionUsages.contains(targetStatement.definitionUsages))
        premiseSimplificationInferences.iterator.findFirst { case (inference, inferencePremise) =>
          (for {
            substitutions <- inferencePremise.calculateSubstitutions(givenPremise, Substitutions.empty, 0, stepContext.externalDepth)
            simplifiedStatement <- inference.conclusion.applySubstitutions(substitutions, 0, stepContext.externalDepth)
            nextSteps <- bySimplifyingPremise(simplifiedStatement)
            assertionStep = Step.Assertion(simplifiedStatement, inference.summary, Seq(Premise.Pending(givenPremise)), substitutions)
          } yield assertionStep +: nextSteps).headOption
        }
      else
        None
    }
    def bySimplifyingPremises = premiseContext.allPremisesSimplestFirst.iterator.map(_.statement).findFirst(bySimplifyingPremise)

    def bySimplifyingTarget = targetSimplificationInferences.iterator.findFirst { inference =>
      (for {
        substitutions <- inference.conclusion.calculateSubstitutions(targetStatement, Substitutions.empty, 0, stepContext.externalDepth)
        premiseStatements <- Try(inference.substitutePremises(substitutions, stepContext)).toOption
        premiseSteps <- findPremiseSteps(premiseStatements, entryContext, premiseContext, stepContext)
        assertionStep = Step.Assertion(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield premiseSteps :+ assertionStep).headOption
    }
    def bySimplifyingTargetComponents = targetStatement match {
      case DefinedStatement(firstComponent +: _, statementDefinition) =>
        (for {
          (simplificationInference, simplificationPremise, firstSimplificationConclusionComponent) <- getStatementDefinitionSimplifications(statementDefinition, entryContext)
          initialSimplificationSubstitutions <- firstSimplificationConclusionComponent.calculateSubstitutions(firstComponent, Substitutions.empty, 0, stepContext.externalDepth)
          (simplificationPremiseSteps, substitutedSimplificationPremise, simplificationSubstitutions) <- findPremiseSteps(simplificationPremise, initialSimplificationSubstitutions, entryContext, premiseContext, stepContext)
          assertionStep = Step.Assertion(
            targetStatement,
            simplificationInference.summary,
            Seq(Premise.Pending(substitutedSimplificationPremise)),
            simplificationSubstitutions)
        } yield simplificationPremiseSteps :+ assertionStep).headOption
      case _ =>
        None
    }

    fromGivenPremises orElse fromFact orElse bySimplifyingPremises orElse bySimplifyingTarget orElse bySimplifyingTargetComponents
  }

  def findPremiseSteps(
    premiseStatements: Seq[Statement],
    entryContext: EntryContext,
    premiseContext: PremiseContext,
    stepContext: StepContext
  ): Option[Seq[Step]] = {
    premiseStatements.map(findPremiseSteps(_, entryContext, premiseContext, stepContext)).traverseOption.map(_.flatten)
  }

  private def getStatementDefinitionSimplifications(statementDefinition: StatementDefinition, entryContext: EntryContext): Seq[(Inference, Statement, Expression)] = {
    entryContext.availableEntries.ofType[Inference].collect {
      case inference @ Inference(
        _,
        Seq(singlePremise @ statementDefinition(firstPremiseComponent, _*)),
        statementDefinition(firstConclusionComponent, _*)
      ) if firstConclusionComponent.complexity > firstPremiseComponent.complexity &&
          firstConclusionComponent.requiredSubstitutions.contains(firstPremiseComponent.requiredSubstitutions) &&
          singlePremise.requiredSubstitutions.contains(inference.requiredSubstitutions)
        =>
        (inference, singlePremise, firstConclusionComponent)
    }
  }

  def findPremiseSteps(
    unsubstitutedPremiseStatement: Statement,
    initialSubstitutions: Substitutions,
    entryContext: EntryContext,
    premiseContext: PremiseContext,
    stepContext: StepContext
  ): Seq[(Seq[Step], Statement, Substitutions)] = {

    def directly = for {
      premise <- premiseContext.allPremisesSimplestFirst
      finalSubstitutions <- unsubstitutedPremiseStatement.calculateSubstitutions(premise.statement, initialSubstitutions, 0, stepContext.externalDepth)
    } yield (Nil, premise.statement, finalSubstitutions)

    def bySimplifying = unsubstitutedPremiseStatement match {
      case DefinedStatement(firstComponent +: _, statementDefinition) =>
        for {
          substitutedFirstComponent <- firstComponent.applySubstitutions(initialSubstitutions, 0, stepContext.externalDepth).toSeq
          (simplificationInference, simplificationPremise, firstSimplificationConclusionComponent) <- getStatementDefinitionSimplifications(statementDefinition, entryContext)
          initialSimplificationSubstitutions <- firstSimplificationConclusionComponent.calculateSubstitutions(substitutedFirstComponent, Substitutions.empty, 0, stepContext.externalDepth)
          (simplificationPremiseSteps, substitutedSimplificationPremise, simplificationSubstitutions) <- findPremiseSteps(simplificationPremise, initialSimplificationSubstitutions, entryContext, premiseContext, stepContext)
          substitutedPremiseStatement <- simplificationInference.conclusion.applySubstitutions(simplificationSubstitutions, 0, stepContext.externalDepth).toSeq
          finalSubstitutions <- unsubstitutedPremiseStatement.calculateSubstitutions(substitutedPremiseStatement, initialSubstitutions, 0, stepContext.externalDepth)
          assertionStep = Step.Assertion(
            substitutedPremiseStatement,
            simplificationInference.summary,
            Seq(Premise.Pending(substitutedSimplificationPremise)),
            simplificationSubstitutions)
        } yield (simplificationPremiseSteps :+ assertionStep, substitutedPremiseStatement, finalSubstitutions)
      case _ =>
        Nil
    }

    directly ++ bySimplifying
  }

  def findPremiseSteps(
    unsubstitutedPremiseStatements: Seq[Statement],
    substitutions: Substitutions,
    entryContext: EntryContext,
    premiseContext: PremiseContext,
    stepContext: StepContext
  ): Option[(Seq[Step], Seq[Statement], Substitutions)] = {

    def helper(remainingUnsubstitutedPremises: Seq[Statement], currentSubstitutions: Substitutions, stepsSoFar: Seq[Step], premisesSoFar: Seq[Statement]): Option[(Seq[Step], Seq[Statement], Substitutions)] = {
      remainingUnsubstitutedPremises match {
        case unsubstitutedPremise +: otherUnsubstitutedPremises =>
          (for {
            (stepsForThisPremise, premise, newSubstitutions) <- findPremiseSteps(unsubstitutedPremise, currentSubstitutions, entryContext, premiseContext, stepContext)
            result <- helper(otherUnsubstitutedPremises, newSubstitutions, stepsSoFar ++ stepsForThisPremise, premisesSoFar :+ premise)
          } yield result).headOption
        case Nil =>
          Some((stepsSoFar, premisesSoFar, currentSubstitutions))
      }
    }
    helper(unsubstitutedPremiseStatements, substitutions, Nil, Nil)
  }
}
