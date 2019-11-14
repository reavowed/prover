package net.prover.model.proof

import net.prover.model.entries.StatementDefinition
import net.prover.model.{EntryContext, Inference, Substitutions}
import net.prover.model.expressions._

import scala.util.Try

object PremiseFinder {

  def findParameterisedPremiseSteps(
    targetStatement: Statement,
    terms: Map[Int, Term])(
    implicit stepContext: StepContext
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
          terms <- targetStatement.calculateArguments(premiseStatement, terms)
        } yield (Nil, terms)
      }
    def bySimplifyingTarget = for {
        inference <- targetSimplificationInferences
        substitutionsWithPlaceholders <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality).toSeq
        premiseStatementsWithPlaceholders <- inference.substitutePremises(substitutionsWithPlaceholders).toSeq
        (premiseSteps, newTerms) <- findParameterisedPremiseSteps(premiseStatementsWithPlaceholders, terms)
        conclusion = targetStatement.specify(newTerms)
        substitutions <- inference.conclusion.calculateSubstitutions(conclusion).flatMap(_.confirmTotality).toSeq
        premiseStatements = premiseStatementsWithPlaceholders.map(_.specify(newTerms))
        assertionStep = Step.Assertion(conclusion, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield (premiseSteps :+ assertionStep, newTerms)

    def asAlreadyKnown = for {
      knownTarget <- Try(targetStatement.specify(terms)).toOption
      steps <- findPremiseSteps(knownTarget)
    } yield (steps, terms)

    asAlreadyKnown.map(Seq(_)) getOrElse (fromGivenPremises ++ bySimplifyingTarget)
  }
  def findParameterisedPremiseSteps(
    targetStatements: Seq[Statement],
    initialTerms: Map[Int, Term])(
    implicit stepContext: StepContext
  ): Seq[(Seq[Step], Map[Int, Term])] = {
    targetStatements.foldLeft(Seq((Seq.empty[Step], initialTerms))) { case (stepsAndTermsSoFar, targetStatement) =>
      for {
        (currentSteps, currentTerms) <- stepsAndTermsSoFar
        (newSteps, newTerms) <- findParameterisedPremiseSteps(targetStatement, currentTerms)
      } yield (currentSteps ++ newSteps, newTerms)
    }
  }

  def findPremiseSteps(
    targetStatement: Statement)(
    implicit stepContext: StepContext
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
    def fromFact = ProofHelper.findFact(targetStatement).map(Seq(_))

    def bySimplifyingPremise(givenPremise: Statement): Option[Seq[Step]] = {
      if (givenPremise == targetStatement)
        Some(Nil)
      else if (givenPremise.complexity > targetStatement.complexity && givenPremise.definitionUsages.contains(targetStatement.definitionUsages))
        premiseSimplificationInferences.iterator.findFirst { case (inference, inferencePremise) =>
          for {
            substitutions <- inferencePremise.calculateSubstitutions(givenPremise).flatMap(_.confirmTotality)
            simplifiedStatement <- inference.conclusion.applySubstitutions(substitutions)
            nextSteps <- bySimplifyingPremise(simplifiedStatement)
            assertionStep = Step.Assertion(simplifiedStatement, inference.summary, Seq(Premise.Pending(givenPremise)), substitutions)
          } yield assertionStep +: nextSteps
        }
      else
        None
    }
    def bySimplifyingPremises = stepContext.allPremisesSimplestFirst.iterator.map(_.statement).findFirst(bySimplifyingPremise)

    def bySimplifyingTarget = targetSimplificationInferences.iterator.findFirst { inference =>
      for {
        substitutions <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality)
        premiseStatements <- inference.substitutePremises(substitutions)
        premiseSteps <- findPremiseSteps(premiseStatements)
        assertionStep = Step.Assertion(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield premiseSteps :+ assertionStep
    }
    def bySimplifyingTargetComponents = targetStatement match {
      case DefinedStatement(_, statementDefinition) =>
        (for {
          (simplificationInference, simplificationPremise, _) <- getStatementDefinitionSimplifications(statementDefinition, stepContext.entryContext)
          initialSimplificationSubstitutions <- simplificationInference.conclusion.calculateSubstitutions(targetStatement).toSeq
          (simplificationPremiseSteps, substitutedSimplificationPremise, possibleSimplificationSubstitutions) <- findPremiseSteps(simplificationPremise, initialSimplificationSubstitutions)
          simplificationSubstitutions <- possibleSimplificationSubstitutions.confirmTotality.toSeq
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
    premiseStatements: Seq[Statement])(
    implicit stepContext: StepContext
  ): Option[Seq[Step]] = {
    premiseStatements.map(findPremiseSteps).traverseOption.map(_.flatten)
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
    initialSubstitutions: Substitutions.Possible)(
    implicit stepContext: StepContext
  ): Seq[(Seq[Step], Statement, Substitutions.Possible)] = {

    def directly = for {
      premise <- stepContext.allPremisesSimplestFirst
      finalSubstitutions <- unsubstitutedPremiseStatement.calculateSubstitutions(premise.statement, initialSubstitutions).toSeq
    } yield (Nil, premise.statement, finalSubstitutions)

    def bySimplifying = unsubstitutedPremiseStatement match {
      case DefinedStatement(firstComponent +: _, statementDefinition) =>
        for {
          substitutedFirstComponent <- firstComponent.applySubstitutions(initialSubstitutions.stripApplications()).toSeq
          (simplificationInference, simplificationPremise, firstSimplificationConclusionComponent) <- getStatementDefinitionSimplifications(statementDefinition, stepContext.entryContext)
          initialSimplificationSubstitutions <- firstSimplificationConclusionComponent.calculateSubstitutions(substitutedFirstComponent).toSeq
          (simplificationPremiseSteps, substitutedSimplificationPremise, possibleSimplificationSubstitutions) <- findPremiseSteps(simplificationPremise, initialSimplificationSubstitutions)
          simplificationSubstitutions <- possibleSimplificationSubstitutions.confirmTotality.toSeq
          substitutedPremiseStatement <- simplificationInference.conclusion.applySubstitutions(simplificationSubstitutions).toSeq
          finalSubstitutions <- unsubstitutedPremiseStatement.calculateSubstitutions(substitutedPremiseStatement, initialSubstitutions).toSeq
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
    substitutions: Substitutions.Possible)(
    implicit stepContext: StepContext
  ): Option[(Seq[Step], Seq[Statement], Substitutions.Possible)] = {
    def helper(remainingUnsubstitutedPremises: Seq[Statement], currentSubstitutions: Substitutions.Possible, stepsSoFar: Seq[Step], premisesSoFar: Seq[Statement]): Option[(Seq[Step], Seq[Statement], Substitutions.Possible)] = {
      remainingUnsubstitutedPremises match {
        case unsubstitutedPremise +: otherUnsubstitutedPremises =>
          (for {
            (stepsForThisPremise, premise, newSubstitutions) <- findPremiseSteps(unsubstitutedPremise, currentSubstitutions)
            result <- helper(otherUnsubstitutedPremises, newSubstitutions, stepsSoFar ++ stepsForThisPremise, premisesSoFar :+ premise)
          } yield result).headOption
        case Nil =>
          Some((stepsSoFar, premisesSoFar, currentSubstitutions))
      }
    }
    helper(unsubstitutedPremiseStatements, substitutions, Nil, Nil)
  }

  def deconstructStatement(statement: Statement)(implicit stepContext: StepContext): (Seq[Statement], Seq[Step]) = {
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
      substitutions <- deconstructionInference.conclusion.calculateSubstitutions(statement).flatMap(_.confirmTotality)
      premises <- deconstructionInference.premises.map(_.applySubstitutions(substitutions)).traverseOption
      premiseStatementsAndSteps = premises.map(deconstructStatement)
      premiseDeconstructedStatements = premiseStatementsAndSteps.flatMap(_._1)
      premiseDeconstructionSteps = premiseStatementsAndSteps.flatMap(_._2)
      step = Step.Assertion(statement, deconstructionInference.summary, premises.map(Premise.Pending), substitutions)
    } yield (premiseDeconstructedStatements, premiseDeconstructionSteps :+ step)

    byDeconstructing.getOrElse((Seq(statement), Nil))
  }
}
