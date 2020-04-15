package net.prover.model.proof

import net.prover.model.expressions._
import net.prover.model.{Inference, Substitutions}

object PremiseFinder {
  def findPremiseStepsForStatement
  (targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[(Statement, Step)]] = {
    findPremiseStepsWithInferencesForStatement(targetStatement).map(_.map { case (statement, step, _) => (statement, step) })
  }

  def findPremiseStepsWithInferencesForStatement(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[(Statement, Step, Inference)]] = {
    stepProvingContext.cachedPremises.getOrElseUpdate(targetStatement.serializedForHash, findPremiseStepsWithInferencesForStatementUncached(targetStatement))
  }

  def findPremiseStepsWithInferencesForStatementUncached(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[(Statement, Step, Inference)]] = {
    import stepProvingContext._

    def fromPremises = allPremiseExtractions.find(_._1 == targetStatement).map(_._2)
    def fromFact = ProofHelper.findFact(targetStatement).map(Seq(_))
    def fromSimplification = allPremiseSimplifications.find(_._1 == targetStatement).map(_._2)

    def bySimplifyingTarget = provingContext.conclusionSimplificationInferences.iterator.findFirst { inference =>
      for {
        substitutions <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality)
        premiseStatements <- inference.substitutePremises(substitutions)
        premiseSteps <- findPremiseStepsWithInferencesForStatements(premiseStatements)
        assertionStep = Step.Assertion(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield premiseSteps :+ (targetStatement, assertionStep, inference)
    }
    def bySimplifyingTargetRelation = provingContext.conclusionRelationSimplificationInferences.iterator.findFirst { conclusionRelationSimplificationInference =>
      for {
        (simplifiedTargets, stepsForInference) <- conclusionRelationSimplificationInference.getConclusionSimplification(targetStatement)
        stepsForSimplifiedTarget <- findPremiseStepsWithInferencesForStatements(simplifiedTargets)
      } yield stepsForSimplifiedTarget ++ stepsForInference
    }

    (fromPremises orElse fromFact orElse fromSimplification orElse bySimplifyingTarget orElse bySimplifyingTargetRelation).map(_.distinctBy(_._1))
  }

  def findPremiseStepsWithInferencesForStatements(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[(Statement, Step, Inference)]] = {
    premiseStatements.map(findPremiseStepsWithInferencesForStatement).traverseOption.map(_.flatten)
  }

  def findPremiseStepsOrTargets(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[Step], Seq[Step.Target]) = {
    val (stepsWithStatement, targets) = premiseStatements.foldLeft((Seq.empty[(Statement, Step)], Seq.empty[Step.Target])) { case ((premiseStepsSoFar, targetStepsSoFar), premiseStatement) =>
      PremiseFinder.findPremiseStepsForStatement(premiseStatement) match {
        case Some(newPremiseSteps) =>
          (premiseStepsSoFar ++ newPremiseSteps, targetStepsSoFar)
        case None =>
          val (deconstructedStatements, deconstructionSteps) = PremiseFinder.deconstructStatement(premiseStatement)
          val (deconstructionTargetSteps, deconstructionPremiseSteps) = deconstructedStatements.foldLeft((Seq.empty[Step.Target], Seq.empty[(Statement, Step)])) { case ((otherTargetStepsSoFar, otherPremiseStepsSoFar), deconstructedStatement) =>
            PremiseFinder.findPremiseStepsForStatement(deconstructedStatement) match {
              case Some(newPremiseSteps) =>
                (otherTargetStepsSoFar, otherPremiseStepsSoFar ++ newPremiseSteps)
              case None =>
                (otherTargetStepsSoFar :+ Step.Target(deconstructedStatement), otherPremiseStepsSoFar)
            }
          }
          (premiseStepsSoFar ++ deconstructionPremiseSteps ++ deconstructionSteps.map(s => s.statement -> s), targetStepsSoFar ++ deconstructionTargetSteps)
      }
    }
    (stepsWithStatement.distinctBy(_._1).map(_._2), targets)
  }

  def findPremiseStepsForStatementBySubstituting(
    unsubstitutedPremiseStatement: Statement,
    initialSubstitutions: Substitutions.Possible)(
    implicit stepProvingContext: StepProvingContext
  ): Seq[(Seq[Step], Statement, Substitutions.Possible)] = {
    import stepProvingContext._

    def directly = for {
      (premise, stepsAndInferences) <- allPremiseExtractions
      finalSubstitutions <- unsubstitutedPremiseStatement.calculateSubstitutions(premise, initialSubstitutions).toSeq
    } yield (stepsAndInferences.map(_._2), premise, finalSubstitutions)

    def fromFact = for {
      (step, statement, _, substitutions) <- ProofHelper.findFactBySubstituting(unsubstitutedPremiseStatement, initialSubstitutions)
    } yield (Seq(step), statement, substitutions)

    def bySimplifying = unsubstitutedPremiseStatement match {
      case DefinedStatement(firstComponent +: _, statementDefinition) =>
        for {
          substitutedFirstComponent <- firstComponent.applySubstitutions(initialSubstitutions.stripApplications()).toSeq
          (simplificationInference, simplificationPremise, firstSimplificationConclusionComponent) <- provingContext.getStatementDefinitionSimplifications(statementDefinition)
          initialSimplificationSubstitutions <- firstSimplificationConclusionComponent.calculateSubstitutions(substitutedFirstComponent).toSeq
          (simplificationPremiseSteps, substitutedSimplificationPremise, possibleSimplificationSubstitutions) <- findPremiseStepsForStatementBySubstituting(simplificationPremise, initialSimplificationSubstitutions)
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
        for {
          statementsAndSteps <- findPremiseStepsForStatement(substitutedPremiseStatement).toSeq
        } yield (statementsAndSteps.distinctBy(_._1).map(_._2), substitutedPremiseStatement, initialSubstitutions)
      case None =>
        directly ++ fromFact ++ bySimplifying
    }
  }

  def findPremiseStepsForStatementsBySubstituting(
    unsubstitutedPremiseStatements: Seq[Statement],
    substitutions: Substitutions.Possible)(
    implicit stepProvingContext: StepProvingContext
  ): Option[(Seq[Step], Seq[Statement], Substitutions.Possible)] = {
    def helper(remainingUnsubstitutedPremises: Seq[Statement], currentSubstitutions: Substitutions.Possible, stepsSoFar: Seq[Step], premisesSoFar: Seq[Statement]): Option[(Seq[Step], Seq[Statement], Substitutions.Possible)] = {
      remainingUnsubstitutedPremises match {
        case unsubstitutedPremise +: otherUnsubstitutedPremises =>
          (for {
            (stepsForThisPremise, premise, newSubstitutions) <- findPremiseStepsForStatementBySubstituting(unsubstitutedPremise, currentSubstitutions)
            result <- helper(otherUnsubstitutedPremises, newSubstitutions, stepsSoFar ++ stepsForThisPremise, premisesSoFar :+ premise)
          } yield result).headOption
        case Nil =>
          Some((stepsSoFar, premisesSoFar, currentSubstitutions))
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
