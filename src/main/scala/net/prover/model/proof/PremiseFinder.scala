package net.prover.model.proof

import net.prover.model.definitions.BinaryRelation
import net.prover.model.expressions._
import net.prover.model.{Inference, Substitutions}

object PremiseFinder {
  def findParameterisedPremiseStepsForStatement(
    targetStatement: Statement,
    terms: Map[Int, Term],
    previousTargets: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Seq[(Seq[Step], Map[Int, Term])] = {
    def fromGivenPremises = stepProvingContext.allPremises
      .map(_.statement)
      .mapCollect { premiseStatement =>
        for {
          terms <- targetStatement.calculateArguments(premiseStatement, terms)
        } yield (Nil, terms)
      }
    def bySimplifyingTarget = for {
        inference <- stepProvingContext.provingContext.conclusionSimplificationInferences
        substitutionsWithPlaceholders <- inference.conclusion.calculateSubstitutions(targetStatement)(SubstitutionContext.withExtraParameter).flatMap(_.confirmTotality).toSeq
        premiseStatementsWithPlaceholders <- inference.substitutePremises(substitutionsWithPlaceholders)(SubstitutionContext.withExtraParameter).toSeq
        (premiseSteps, newTerms) <- findParameterisedPremiseStepsForStatements(premiseStatementsWithPlaceholders, terms, previousTargets)
        conclusion <- targetStatement.specify(newTerms).toSeq
        substitutions <- inference.conclusion.calculateSubstitutions(conclusion).flatMap(_.confirmTotality).toSeq
        premiseStatements <- premiseStatementsWithPlaceholders.map(_.specify(newTerms)).traverseOption
        assertionStep = Step.Assertion(conclusion, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield (premiseSteps :+ assertionStep, newTerms)

    def asAlreadyKnown = for {
      knownTarget <- targetStatement.specify(terms)
      steps <- findPremiseStepsForStatement(knownTarget, previousTargets)
    } yield (steps, terms)

    asAlreadyKnown.map(Seq(_)) getOrElse (fromGivenPremises ++ bySimplifyingTarget)
  }
  def findParameterisedPremiseStepsForStatements(
    targetStatements: Seq[Statement],
    initialTerms: Map[Int, Term],
    previousTargets: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Seq[(Seq[Step], Map[Int, Term])] = {
    targetStatements.foldLeft(Seq((Seq.empty[Step], initialTerms))) { case (stepsAndTermsSoFar, targetStatement) =>
      for {
        (currentSteps, currentTerms) <- stepsAndTermsSoFar
        (newSteps, newTerms) <- findParameterisedPremiseStepsForStatement(targetStatement, currentTerms, previousTargets)
      } yield (currentSteps ++ newSteps, newTerms)
    }
  }

  def findPremiseStepsForStatement(
    targetStatement: Statement,
    previousTargets: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[Step]] = {
    findPremiseStepsWithInferencesForStatement(targetStatement, previousTargets).map(_.map(_._1))
  }

  def findPremiseStepsWithInferencesForStatement(
    targetStatement: Statement,
    previousTargets: Seq[Statement],
    currentRelation: Option[BinaryRelation] = None)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[(Step, Inference)]] = {
    import stepProvingContext._

    def fromPremises = allPremiseExtractions.find(_._1 == targetStatement).map(_._2)
    def fromFact = ProofHelper.findFact(targetStatement).map(Seq(_))
    def fromSimplification = allPremiseSimplifications.find(_._1 == targetStatement).map(_._2)

    def fromDoubleSimplifiedConclusionRelations = (for {
      conclusionRelationSimplificationInference <- provingContext.conclusionRelationSimplificationInferences.iterator.filter(i => !currentRelation.exists(_ != i.conclusionRelation))
      (simplifiedTargets, step) <- conclusionRelationSimplificationInference.matchTarget(targetStatement, previousTargets :+ targetStatement)
      innerSteps <- simplifiedTargets.map { case (target, relation) =>
        findPremiseStepsWithInferencesForStatement(target, previousTargets :+ targetStatement, Some(relation))
      }.traverseOption.map(_.flatten)
    } yield innerSteps :+ (step, conclusionRelationSimplificationInference.inference)).headOption

    def bySimplifyingTarget = provingContext.conclusionSimplificationInferences.iterator.findFirst { inference =>
      for {
        substitutions <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality)
        premiseStatements <- inference.substitutePremises(substitutions)
        premiseSteps <- findPremiseStepsWithInferencesForStatements(premiseStatements, previousTargets)
        assertionStep = Step.Assertion(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield premiseSteps :+ (assertionStep, inference)
    }

    if (previousTargets.contains(targetStatement))
      None
    else
      fromPremises orElse fromFact orElse fromSimplification orElse bySimplifyingTarget orElse fromDoubleSimplifiedConclusionRelations
  }

  def findPremiseStepsForStatements(
    premiseStatements: Seq[Statement],
    previousTargets: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[Step]] = {
    findPremiseStepsWithInferencesForStatements(premiseStatements, previousTargets).map(_.map(_._1))
  }

  def findPremiseStepsWithInferencesForStatements(
    premiseStatements: Seq[Statement],
    previousTargets: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[(Step, Inference)]] = {
    premiseStatements.map(findPremiseStepsWithInferencesForStatement(_, previousTargets)).traverseOption.map(_.flatten)
  }

  def findPremiseStepsOrTargets(
    premiseStatements: Seq[Statement],
    previousTargets: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[Step], Seq[Step.Target]) = {
    premiseStatements.foldLeft((Seq.empty[Step], Seq.empty[Step.Target])) { case ((premiseStepsSoFar, targetStepsSoFar), premiseStatement) =>
      PremiseFinder.findPremiseStepsForStatement(premiseStatement, previousTargets) match {
        case Some(newPremiseSteps) =>
          (premiseStepsSoFar ++ newPremiseSteps, targetStepsSoFar)
        case None =>
          val (deconstructedStatements, deconstructionSteps) = PremiseFinder.deconstructStatement(premiseStatement)
          val (deconstructionTargetSteps, deconstructionPremiseSteps) = deconstructedStatements.foldLeft((Seq.empty[Step.Target], Seq.empty[Step])) { case ((otherTargetStepsSoFar, otherPremiseStepsSoFar), deconstructedStatement) =>
            PremiseFinder.findPremiseStepsForStatement(deconstructedStatement, previousTargets) match {
              case Some(newPremiseSteps) =>
                (otherTargetStepsSoFar, otherPremiseStepsSoFar ++ newPremiseSteps)
              case None =>
                (otherTargetStepsSoFar :+ Step.Target(deconstructedStatement), otherPremiseStepsSoFar)
            }
          }
          (premiseStepsSoFar ++ deconstructionPremiseSteps ++ deconstructionSteps, targetStepsSoFar ++ deconstructionTargetSteps)
      }
    }
  }

  def findPremiseStepsForStatementBySubstituting(
    unsubstitutedPremiseStatement: Statement,
    initialSubstitutions: Substitutions.Possible,
    previousTargets: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Seq[(Seq[Step], Statement, Substitutions.Possible)] = {
    import stepProvingContext._

    def directly = for {
      (premise, stepsAndInferences) <- allPremiseExtractions
      finalSubstitutions <- unsubstitutedPremiseStatement.calculateSubstitutions(premise, initialSubstitutions).toSeq
    } yield (stepsAndInferences.map(_._1), premise, finalSubstitutions)

    def bySimplifying = unsubstitutedPremiseStatement match {
      case DefinedStatement(firstComponent +: _, statementDefinition) =>
        for {
          substitutedFirstComponent <- firstComponent.applySubstitutions(initialSubstitutions.stripApplications()).toSeq
          (simplificationInference, simplificationPremise, firstSimplificationConclusionComponent) <- provingContext.getStatementDefinitionSimplifications(statementDefinition)
          initialSimplificationSubstitutions <- firstSimplificationConclusionComponent.calculateSubstitutions(substitutedFirstComponent).toSeq
          (simplificationPremiseSteps, substitutedSimplificationPremise, possibleSimplificationSubstitutions) <- findPremiseStepsForStatementBySubstituting(simplificationPremise, initialSimplificationSubstitutions, previousTargets)
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

    unsubstitutedPremiseStatement.applySubstitutions(initialSubstitutions.stripApplications()) match {
      case Some(substitutedPremiseStatement) =>
        findPremiseStepsForStatement(substitutedPremiseStatement, previousTargets).map((_, substitutedPremiseStatement, initialSubstitutions)).toSeq
      case None =>
        directly ++ bySimplifying
    }
  }

  def findPremiseStepsForStatementsBySubstituting(
    unsubstitutedPremiseStatements: Seq[Statement],
    substitutions: Substitutions.Possible,
    previousTargets: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Option[(Seq[Step], Seq[Statement], Substitutions.Possible)] = {
    def helper(remainingUnsubstitutedPremises: Seq[Statement], currentSubstitutions: Substitutions.Possible, stepsSoFar: Seq[Step], premisesSoFar: Seq[Statement]): Option[(Seq[Step], Seq[Statement], Substitutions.Possible)] = {
      remainingUnsubstitutedPremises match {
        case unsubstitutedPremise +: otherUnsubstitutedPremises =>
          (for {
            (stepsForThisPremise, premise, newSubstitutions) <- findPremiseStepsForStatementBySubstituting(unsubstitutedPremise, currentSubstitutions, previousTargets)
            result <- helper(otherUnsubstitutedPremises, newSubstitutions, stepsSoFar ++ stepsForThisPremise, premisesSoFar :+ premise)
          } yield result).headOption
        case Nil =>
          Some((stepsSoFar, premisesSoFar, currentSubstitutions))
      }
    }
    helper(unsubstitutedPremiseStatements, substitutions, Nil, Nil)
  }

  def findPremiseStepsOrTargetsForStatementsBySubstituting(
    unsubstitutedPremiseStatements: Seq[Statement],
    substitutions: Substitutions.Possible,
    previousTargets: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Option[(Seq[Step], Seq[Step.Target], Seq[Statement], Substitutions.Possible)] = {
    def helper(remainingUnsubstitutedPremises: Seq[Statement], currentSubstitutions: Substitutions.Possible, stepsSoFar: Seq[Step], premisesSoFar: Seq[Either[Statement, Statement]]): Option[(Seq[Step], Seq[Step.Target], Seq[Statement], Substitutions.Possible)] = {
      remainingUnsubstitutedPremises match {
        case unsubstitutedPremise +: otherUnsubstitutedPremises =>
          val resultsForThisPremise = for {
            (premise, stepsAndInferences) <- stepProvingContext.allPremiseExtractions
            finalSubstitutions <- unsubstitutedPremise.calculateSubstitutions(premise, currentSubstitutions).toSeq
          } yield (stepsAndInferences.map(_._1), premise, finalSubstitutions)
          if (resultsForThisPremise.nonEmpty) {
            (for {
              (stepsForThisPremise, premise, newSubstitutions) <- resultsForThisPremise
              result <- helper(otherUnsubstitutedPremises, newSubstitutions, stepsSoFar ++ stepsForThisPremise, premisesSoFar :+ Left(premise))
            } yield result).headOption
          } else {
            helper(otherUnsubstitutedPremises, currentSubstitutions, stepsSoFar, premisesSoFar :+ Right(unsubstitutedPremise))
          }
        case Nil =>
          for {
            statementsAndTargets <- premisesSoFar.map {
              case Left(statement) => Some((statement, None))
              case Right(statement) => statement.applySubstitutions(currentSubstitutions.stripApplications()).map { s => (s, Some(Step.Target(s)))}
            }.traverseOption
            statements = statementsAndTargets.map(_._1)
            targets = statementsAndTargets.flatMap(_._2)
          } yield (stepsSoFar, targets, statements, currentSubstitutions)
      }
    }
    helper(unsubstitutedPremiseStatements, substitutions, Nil, Nil)
  }

  private def deconstructStatement(
    statement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[Statement], Seq[Step.Assertion]) = {
    import stepProvingContext._
    def byDeconstructing = (for {
      deconstructionInference <- provingContext.statementDefinitionDeconstructions
      substitutions <- deconstructionInference.conclusion.calculateSubstitutions(statement).flatMap(_.confirmTotality)
      premises <- deconstructionInference.premises.map(_.applySubstitutions(substitutions)).traverseOption
      premiseStatementsAndSteps = premises.map(deconstructStatement)
      premiseDeconstructedStatements = premiseStatementsAndSteps.flatMap(_._1)
      premiseDeconstructionSteps = premiseStatementsAndSteps.flatMap(_._2)
      step = Step.Assertion(statement, deconstructionInference.summary, premises.map(Premise.Pending), substitutions)
    } yield (premiseDeconstructedStatements, premiseDeconstructionSteps :+ step)).headOption

    byDeconstructing.getOrElse((Seq(statement), Nil))
  }
}
