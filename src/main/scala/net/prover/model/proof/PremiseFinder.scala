package net.prover.model.proof

import net.prover.model.entries.StatementDefinition
import net.prover.model.{EntryContext, Inference, Substitutions}
import net.prover.model.expressions._

import scala.util.Try

object PremiseFinder {

  def findParameterisedPremiseSteps(
    targetStatement: Statement,
    terms: Map[Int, Term],
    stepContext: StepContext
  ): Seq[(Seq[Step], Map[Int, Term])] = {
    val targetSimplificationInferences = stepContext.entryContext.availableEntries.ofType[Inference].filter {
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

    def fromGivenPremises = stepContext.allPremisesSimplestFirst
      .map(_.statement)
      .mapCollect { premiseStatement =>
        for {
          terms <- targetStatement.calculateArguments(premiseStatement, terms, 0, stepContext.externalDepth)
        } yield (Nil, terms)
      }
    def bySimplifyingTarget = for {
        inference <- targetSimplificationInferences
        substitutionsWithPlaceholders <- inference.conclusion.calculateSubstitutions(targetStatement, Substitutions.empty, 0, stepContext.externalDepth)
        premiseStatementsWithPlaceholders <- Try(inference.substitutePremises(substitutionsWithPlaceholders, stepContext)).toOption.toSeq
        (premiseSteps, newTerms) <- findParameterisedPremiseSteps(premiseStatementsWithPlaceholders, terms, stepContext)
        conclusion <- targetStatement.specify(newTerms, 0, stepContext.externalDepth).toSeq
        substitutions <- inference.conclusion.calculateSubstitutions(conclusion, Substitutions.empty, 0, stepContext.externalDepth)
        premiseStatements <- premiseStatementsWithPlaceholders.map(_.specify(newTerms, 0, stepContext.externalDepth)).traverseOption.toSeq
        assertionStep = Step.Assertion(conclusion, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield (premiseSteps :+ assertionStep, newTerms)

    def asAlreadyKnown = for {
      knownTarget <- targetStatement.specify(terms, 0, stepContext.externalDepth)
      steps <- findPremiseSteps(knownTarget, stepContext)
    } yield (steps, terms)

    asAlreadyKnown.map(Seq(_)) getOrElse (fromGivenPremises ++ bySimplifyingTarget)
  }
  def findParameterisedPremiseSteps(
    targetStatements: Seq[Statement],
    initialTerms: Map[Int, Term],
    stepContext: StepContext
  ): Seq[(Seq[Step], Map[Int, Term])] = {
    targetStatements.foldLeft(Seq((Seq.empty[Step], initialTerms))) { case (stepsAndTermsSoFar, targetStatement) =>
      for {
        (currentSteps, currentTerms) <- stepsAndTermsSoFar
        (newSteps, newTerms) <- findParameterisedPremiseSteps(targetStatement, currentTerms, stepContext)
      } yield (currentSteps ++ newSteps, newTerms)
    }
  }

  def findPremiseSteps(
    targetStatement: Statement,
    stepContext: StepContext
  ): Option[Seq[Step]] = {
    val premiseSimplificationInferences = stepContext.entryContext.availableEntries.ofType[Inference].collect {
      case inference @ Inference(_, Seq(singlePremise), conclusion)
        if singlePremise.complexity > conclusion.complexity &&
          singlePremise.requiredSubstitutions.isEquivalentTo(inference.requiredSubstitutions) &&
          singlePremise.requiredSubstitutions.predicates.isEmpty && singlePremise.requiredSubstitutions.functions.isEmpty &&
          conclusion.referencedDefinitions.subsetOf(singlePremise.referencedDefinitions)
      =>
        (inference, singlePremise)
    }
    val targetSimplificationInferences = stepContext.entryContext.availableEntries.ofType[Inference].filter {
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

    def fromGivenPremises = stepContext.allPremisesSimplestFirst
      .map(_.statement)
      .find(_ == targetStatement)
      .map(_ => Nil)
    def fromFact = ProofHelper.findFact(targetStatement, stepContext).map(Seq(_))

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
    def bySimplifyingPremises = stepContext.allPremisesSimplestFirst.iterator.map(_.statement).findFirst(bySimplifyingPremise)

    def bySimplifyingTarget = targetSimplificationInferences.iterator.findFirst { inference =>
      (for {
        substitutions <- inference.conclusion.calculateSubstitutions(targetStatement, Substitutions.empty, 0, stepContext.externalDepth)
        premiseStatements <- Try(inference.substitutePremises(substitutions, stepContext)).toOption
        premiseSteps <- findPremiseSteps(premiseStatements, stepContext)
        assertionStep = Step.Assertion(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield premiseSteps :+ assertionStep).headOption
    }
    def bySimplifyingTargetComponents = targetStatement match {
      case DefinedStatement(firstComponent +: _, statementDefinition) =>
        (for {
          (simplificationInference, simplificationPremise, firstSimplificationConclusionComponent) <- getStatementDefinitionSimplifications(statementDefinition, stepContext.entryContext)
          initialSimplificationSubstitutions <- firstSimplificationConclusionComponent.calculateSubstitutions(firstComponent, Substitutions.empty, 0, stepContext.externalDepth)
          (simplificationPremiseSteps, substitutedSimplificationPremise, simplificationSubstitutions) <- findPremiseSteps(simplificationPremise, initialSimplificationSubstitutions, stepContext)
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
    stepContext: StepContext
  ): Option[Seq[Step]] = {
    premiseStatements.map(findPremiseSteps(_, stepContext)).traverseOption.map(_.flatten)
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
    stepContext: StepContext
  ): Seq[(Seq[Step], Statement, Substitutions)] = {

    def directly = for {
      premise <- stepContext.allPremisesSimplestFirst
      finalSubstitutions <- unsubstitutedPremiseStatement.calculateSubstitutions(premise.statement, initialSubstitutions, 0, stepContext.externalDepth)
    } yield (Nil, premise.statement, finalSubstitutions)

    def bySimplifying = unsubstitutedPremiseStatement match {
      case DefinedStatement(firstComponent +: _, statementDefinition) =>
        for {
          substitutedFirstComponent <- firstComponent.applySubstitutions(initialSubstitutions, 0, stepContext.externalDepth).toSeq
          (simplificationInference, simplificationPremise, firstSimplificationConclusionComponent) <- getStatementDefinitionSimplifications(statementDefinition, stepContext.entryContext)
          initialSimplificationSubstitutions <- firstSimplificationConclusionComponent.calculateSubstitutions(substitutedFirstComponent, Substitutions.empty, 0, stepContext.externalDepth)
          (simplificationPremiseSteps, substitutedSimplificationPremise, simplificationSubstitutions) <- findPremiseSteps(simplificationPremise, initialSimplificationSubstitutions, stepContext)
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
    stepContext: StepContext
  ): Option[(Seq[Step], Seq[Statement], Substitutions)] = {

    def helper(remainingUnsubstitutedPremises: Seq[Statement], currentSubstitutions: Substitutions, stepsSoFar: Seq[Step], premisesSoFar: Seq[Statement]): Option[(Seq[Step], Seq[Statement], Substitutions)] = {
      remainingUnsubstitutedPremises match {
        case unsubstitutedPremise +: otherUnsubstitutedPremises =>
          (for {
            (stepsForThisPremise, premise, newSubstitutions) <- findPremiseSteps(unsubstitutedPremise, currentSubstitutions, stepContext)
            result <- helper(otherUnsubstitutedPremises, newSubstitutions, stepsSoFar ++ stepsForThisPremise, premisesSoFar :+ premise)
          } yield result).headOption
        case Nil =>
          Some((stepsSoFar, premisesSoFar, currentSubstitutions))
      }
    }
    helper(unsubstitutedPremiseStatements, substitutions, Nil, Nil)
  }

  def deconstructStatement(statement: Statement, stepContext: StepContext): (Seq[Statement], Seq[Step]) = {
    def byDeconstructing = for {
      statementDefinition <- statement.asOptionalInstanceOf[DefinedStatement].map(_.definition)
      deconstructionInference <- stepContext.entryContext.inferences.find {
        case Inference(_, premises, DefinedStatement(components, `statementDefinition`))
          if components.map(_.asOptionalInstanceOf[StatementVariable]).traverseOption.contains(premises)
        =>
          true
        case _ =>
          false
      }
      substitutions <- deconstructionInference.conclusion.calculateSubstitutions(statement, Substitutions.empty, 0, stepContext.externalDepth).headOption
      premises <- deconstructionInference.premises.map(_.applySubstitutions(substitutions, 0, stepContext.externalDepth)).traverseOption
      premiseStatementsAndSteps = premises.map(deconstructStatement(_, stepContext))
      premiseDeconstructedStatements = premiseStatementsAndSteps.flatMap(_._1)
      premiseDeconstructionSteps = premiseStatementsAndSteps.flatMap(_._2)
      step = Step.Assertion(statement, deconstructionInference.summary, premises.map(Premise.Pending), substitutions)
    } yield (premiseDeconstructedStatements, premiseDeconstructionSteps :+ step)

    byDeconstructing.getOrElse((Seq(statement), Nil))
  }
}
