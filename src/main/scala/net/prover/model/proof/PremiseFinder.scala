package net.prover.model.proof

import net.prover.model.Substitutions
import net.prover.model.expressions._

import scala.Ordering.Implicits._

object PremiseFinder {
  def findParameterisedPremiseSteps(
    targetStatement: Statement,
    terms: Map[Int, Term])(
    implicit stepProvingContext: StepProvingContext
  ): Seq[(Seq[Step], Map[Int, Term])] = {
    def fromGivenPremises = stepProvingContext.premisesThenSimplifications
      .map(_.statement)
      .mapCollect { premiseStatement =>
        for {
          terms <- targetStatement.calculateArguments(premiseStatement, terms)
        } yield (Nil, terms)
      }
    def bySimplifyingTarget = for {
        inference <- stepProvingContext.provingContext.conclusionSimplificationInferences
        substitutionsWithPlaceholders <- inference.conclusion.calculateSubstitutions(targetStatement)(StepContext.withExtraParameter).flatMap(_.confirmTotality).toSeq
        premiseStatementsWithPlaceholders <- inference.substitutePremises(substitutionsWithPlaceholders)(StepContext.withExtraParameter).toSeq
        (premiseSteps, newTerms) <- findParameterisedPremiseSteps(premiseStatementsWithPlaceholders, terms)
        conclusion <- targetStatement.specify(newTerms).toSeq
        substitutions <- inference.conclusion.calculateSubstitutions(conclusion).flatMap(_.confirmTotality).toSeq
        premiseStatements <- premiseStatementsWithPlaceholders.map(_.specify(newTerms)).traverseOption
        assertionStep = Step.Assertion(conclusion, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield (premiseSteps :+ assertionStep, newTerms)

    def asAlreadyKnown = for {
      knownTarget <- targetStatement.specify(terms)
      steps <- findPremiseSteps(knownTarget)
    } yield (steps, terms)

    asAlreadyKnown.map(Seq(_)) getOrElse (fromGivenPremises ++ bySimplifyingTarget)
  }
  def findParameterisedPremiseSteps(
    targetStatements: Seq[Statement],
    initialTerms: Map[Int, Term])(
    implicit stepProvingContext: StepProvingContext
  ): Seq[(Seq[Step], Map[Int, Term])] = {
    targetStatements.foldLeft(Seq((Seq.empty[Step], initialTerms))) { case (stepsAndTermsSoFar, targetStatement) =>
      for {
        (currentSteps, currentTerms) <- stepsAndTermsSoFar
        (newSteps, newTerms) <- findParameterisedPremiseSteps(targetStatement, currentTerms)
      } yield (currentSteps ++ newSteps, newTerms)
    }
  }

  private def findPremiseStepsWithoutRewriting(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[Step.Assertion]] = {
    import stepProvingContext._
    def fromGivenPremises = premisesThenSimplifications
      .find(_.statement == targetStatement)
      .map(_ => Nil)
    def fromFact = ProofHelper.findFact(targetStatement).map(Seq(_))

    def bySimplifyingPremise(givenPremise: Statement): Option[Seq[Step.Assertion]] = {
      def byMatchingDirectly = {
        if (givenPremise == targetStatement)
          Some(Nil)
        else
          None
      }
      def bySimplifying = {
        if (givenPremise.complexity > targetStatement.complexity && givenPremise.definitionUsages.contains(targetStatement.definitionUsages))
          provingContext.premiseSimplificationInferences.iterator.findFirst { case (inference, inferencePremise) =>
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
      def bySimplifyingComponents = (givenPremise, targetStatement) match {
        case (DefinedStatement(firstPremiseComponent +: _, premiseStatementDefinition), DefinedStatement(firstTargetComponent +: _, targetStatementDefinition))
          if premiseStatementDefinition == targetStatementDefinition && firstTargetComponent.complexity > firstPremiseComponent.complexity
        =>
          (for {
            (simplificationInference, simplificationPremise, simplificationConclusionComponent) <- provingContext.getStatementDefinitionSimplifications(premiseStatementDefinition)
            if simplificationConclusionComponent.referencedDefinitions.subsetOf(firstTargetComponent.referencedDefinitions)
            substitutions <- simplificationPremise.calculateSubstitutions(givenPremise).flatMap(_.confirmTotality).toSeq
            simplifiedPremise <- simplificationInference.conclusion.applySubstitutions(substitutions).toSeq
            nextSteps <- bySimplifyingPremise(simplifiedPremise)
            assertionStep = Step.Assertion(simplifiedPremise, simplificationInference.summary, Seq(Premise.Pending(givenPremise)), substitutions)
          } yield assertionStep +: nextSteps).headOption
        case _ =>
          None
      }

      byMatchingDirectly orElse bySimplifying orElse bySimplifyingComponents
    }
    def bySimplifyingPremises = allPremisesSimplestFirst.iterator.map(_.statement).findFirst(bySimplifyingPremise)

    def bySimplifyingTarget = provingContext.conclusionSimplificationInferences.iterator.findFirst { inference =>
      for {
        substitutions <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality)
        premiseStatements <- inference.substitutePremises(substitutions)
        premiseSteps <- findPremiseSteps(premiseStatements)
        assertionStep = Step.Assertion(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield premiseSteps :+ assertionStep
    }

    fromGivenPremises orElse fromFact orElse bySimplifyingPremises orElse bySimplifyingTarget
  }

  private def findPremiseStepsWithRewriting(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[Step.Assertion]] = {
    stepProvingContext.provingContext.rewriteInferences.mapFind { case (inference, singlePremise) =>
      for {
        rewriteSubstitutions <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality)
        rewrittenStatement <- singlePremise.applySubstitutions(rewriteSubstitutions)
        innerSteps <- findPremiseStepsWithoutRewriting(rewrittenStatement)
        rewriteStep = Step.Assertion(
          targetStatement,
          inference.summary,
          Seq(Premise.Pending(rewrittenStatement)),
          rewriteSubstitutions)
      } yield innerSteps :+ rewriteStep
    }
  }

  def findPremiseSteps(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[Step.Assertion]] = {
    findPremiseStepsWithoutRewriting(targetStatement) orElse findPremiseStepsWithRewriting(targetStatement)
  }

  def findPremiseSteps(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[Step.Assertion]] = {
    premiseStatements.map(findPremiseSteps).traverseOption.map(_.flatten)
  }

  def findPremiseStepsOrTargets(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[Step.Assertion], Seq[Step.Target]) = {
    premiseStatements.foldLeft((Seq.empty[Step.Assertion], Seq.empty[Step.Target])) { case ((premiseStepsSoFar, targetStepsSoFar), premiseStatement) =>
      PremiseFinder.findPremiseSteps(premiseStatement) match {
        case Some(newPremiseSteps) =>
          (premiseStepsSoFar ++ newPremiseSteps, targetStepsSoFar)
        case None =>
          val (deconstructedStatements, deconstructionSteps) = PremiseFinder.deconstructStatement(premiseStatement)
          val (deconstructionTargetSteps, deconstructionPremiseSteps) = deconstructedStatements.foldLeft((Seq.empty[Step.Target], Seq.empty[Step.Assertion])) { case ((otherTargetStepsSoFar, otherPremiseStepsSoFar), deconstructedStatement) =>
            PremiseFinder.findPremiseSteps(deconstructedStatement) match {
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


  def findPremiseSteps(
    unsubstitutedPremiseStatement: Statement,
    initialSubstitutions: Substitutions.Possible)(
    implicit stepProvingContext: StepProvingContext
  ): Seq[(Seq[Step.Assertion], Statement, Substitutions.Possible)] = {
    import stepProvingContext._

    def directly = for {
      premise <- allPremisesSimplestFirst
      finalSubstitutions <- unsubstitutedPremiseStatement.calculateSubstitutions(premise.statement, initialSubstitutions).toSeq
    } yield (Nil, premise.statement, finalSubstitutions)

    def bySimplifying = unsubstitutedPremiseStatement match {
      case DefinedStatement(firstComponent +: _, statementDefinition) =>
        for {
          substitutedFirstComponent <- firstComponent.applySubstitutions(initialSubstitutions.stripApplications()).toSeq
          (simplificationInference, simplificationPremise, firstSimplificationConclusionComponent) <- provingContext.getStatementDefinitionSimplifications(statementDefinition)
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

    unsubstitutedPremiseStatement.applySubstitutions(initialSubstitutions.stripApplications()) match {
      case Some(substitutedPremiseStatement) =>
        findPremiseSteps(substitutedPremiseStatement).map((_, substitutedPremiseStatement, initialSubstitutions)).toSeq
      case None =>
        directly ++ bySimplifying
    }
  }

  def findPremiseSteps(
    unsubstitutedPremiseStatements: Seq[Statement],
    substitutions: Substitutions.Possible)(
    implicit stepProvingContext: StepProvingContext
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

  def deconstructStatement(
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
