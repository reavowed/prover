package net.prover.model.proof

import net.prover.controllers.ExtractionHelper
import net.prover.model.Substitutions
import net.prover.model.definitions.TermDefinition
import net.prover.model.expressions._

object PremiseFinder {

  def findPremiseStepsForStatement(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[DerivationStep]] = {
    stepProvingContext.cachedPremises.getOrElseUpdate(
      targetStatement.serializedForHash,
      findPremiseStepsWithInferencesForStatementUncached(targetStatement))
  }

  def findPremiseStepsWithInferencesForStatementUncached(
    targetStatement: Statement)(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[DerivationStep]] = {
    import stepProvingContext._

    def fromPremises = premiseSimplificationsBySerializedStatement.get(targetStatement.serialized)
    def fromFact = ProofHelper.findFact(targetStatement).map(Seq(_))

    def byRemovingTermDefinition = (for {
      termDefinition <- targetStatement.referencedDefinitions.ofType[TermDefinition].iterator
      extractionOption <- provingContext.termDefinitionRemovals(termDefinition)
      substitutions <- extractionOption.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality)
      premiseStatements <- extractionOption.premises.map(_.applySubstitutions(substitutions)).traverseOption
      premiseSteps <- findPremiseStepsForStatements(premiseStatements)
      derivationStep <- ExtractionHelper.getInferenceExtractionWithoutPremises(termDefinition.definitionInference, substitutions, extractionOption)
    } yield premiseSteps :+ derivationStep).headOption

    def bySimplifyingTarget = provingContext.conclusionSimplificationInferences.iterator.findFirst { inference =>
      for {
        substitutions <- inference.conclusion.calculateSubstitutions(targetStatement).flatMap(_.confirmTotality)
        premiseStatements <- inference.substitutePremises(substitutions)
        premiseSteps <- findPremiseStepsForStatements(premiseStatements)
        assertionStep = Step.Assertion(targetStatement, inference.summary, premiseStatements.map(Premise.Pending), substitutions)
      } yield premiseSteps :+ DerivationStep(targetStatement, inference, assertionStep)
    }
    def bySimplifyingTargetRelation = provingContext.conclusionRelationSimplificationInferences.iterator.findFirst { conclusionRelationSimplificationInference =>
      for {
        (simplifiedTargets, stepsForInference) <- conclusionRelationSimplificationInference.getConclusionSimplification(targetStatement)
        stepsForSimplifiedTarget <- findPremiseStepsForStatements(simplifiedTargets)
      } yield stepsForSimplifiedTarget ++ stepsForInference
    }

    (fromPremises orElse fromFact orElse byRemovingTermDefinition orElse bySimplifyingTarget orElse bySimplifyingTargetRelation).map(_.deduplicate)
  }

  def findPremiseStepsForStatements(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Option[Seq[DerivationStep]] = {
    premiseStatements.map(findPremiseStepsForStatement).traverseOption.map(_.flatten)
  }

  def findPremiseStepsOrTargets(
    premiseStatements: Seq[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): (Seq[DerivationStep], Seq[Step.Target]) = {
    val (premiseSteps, targets) = premiseStatements.foldLeft((Seq.empty[DerivationStep], Seq.empty[Step.Target])) { case ((premiseStepsSoFar, targetStepsSoFar), premiseStatement) =>
      PremiseFinder.findPremiseStepsForStatement(premiseStatement) match {
        case Some(newPremiseSteps) =>
          (premiseStepsSoFar ++ newPremiseSteps, targetStepsSoFar)
        case None =>
          val (deconstructedStatements, deconstructionSteps) = PremiseFinder.deconstructStatement(premiseStatement)
          val (deconstructionTargetSteps, deconstructionPremiseSteps) = deconstructedStatements.foldLeft((Seq.empty[Step.Target], Seq.empty[DerivationStep])) { case ((otherTargetStepsSoFar, otherPremiseStepsSoFar), deconstructedStatement) =>
            PremiseFinder.findPremiseStepsForStatement(deconstructedStatement) match {
              case Some(newPremiseSteps) =>
                (otherTargetStepsSoFar, otherPremiseStepsSoFar ++ newPremiseSteps)
              case None =>
                (otherTargetStepsSoFar :+ Step.Target(deconstructedStatement), otherPremiseStepsSoFar)
            }
          }
          (premiseStepsSoFar ++ deconstructionPremiseSteps ++ deconstructionSteps.map(DerivationStep.fromAssertion), targetStepsSoFar ++ deconstructionTargetSteps)
      }
    }
    (premiseSteps.deduplicate, targets)
  }

  def findPremiseStepsForStatementBySubstituting(
    unsubstitutedPremiseStatement: Statement,
    initialSubstitutions: Substitutions.Possible,
    availablePremises: Seq[(Statement, Seq[DerivationStep])])(
    implicit stepProvingContext: StepProvingContext
  ): Seq[(Seq[DerivationStep], Statement, Substitutions.Possible)] = {
    import stepProvingContext._

    def directly = for {
      (premise, premiseSteps) <- availablePremises
      finalSubstitutions <- unsubstitutedPremiseStatement.calculateSubstitutions(premise, initialSubstitutions).toSeq
    } yield (premiseSteps, premise, finalSubstitutions)

    def fromFact = for {
      (premiseStep, substitutions) <- ProofHelper.findFactBySubstituting(unsubstitutedPremiseStatement, initialSubstitutions)
    } yield (Seq(premiseStep), premiseStep.statement, substitutions)

    def byDeconstructing = for {
      deconstructionInference <- provingContext.statementDefinitionDeconstructions
      initialDeconstructionSubstitutions <- deconstructionInference.conclusion.calculateSubstitutions(unsubstitutedPremiseStatement).flatMap(_.confirmTotality)
      deconstructedUnsubstitutedPremiseStatements <- deconstructionInference.premises.map(_.applySubstitutions(initialDeconstructionSubstitutions)).traverseOption
      (innerPremiseSteps, deconstructedPremiseStatements, innerSubstitutions) <- findPremiseStepsForStatementsBySubstituting(deconstructedUnsubstitutedPremiseStatements, initialSubstitutions, availablePremises)
      deconstructionPremisesWithDeconstructedStatements <- deconstructionInference.premises.zipStrict(deconstructedPremiseStatements)
      finalDeconstructionSubstitutions <- deconstructionPremisesWithDeconstructedStatements.foldLeft(Option(Substitutions.Possible.empty)) { case (substitutionsOption, (premise, statement)) =>
        substitutionsOption.flatMap(premise.calculateSubstitutions(statement, _))
      }.flatMap(_.confirmTotality)
      deconstructionStep <- Step.Assertion.forInference(deconstructionInference, finalDeconstructionSubstitutions)
    } yield (innerPremiseSteps :+ DerivationStep.fromAssertion(deconstructionStep), deconstructionStep.statement, innerSubstitutions)

    directly ++ fromFact ++ byDeconstructing
  }

  def findPremiseStepsForStatementsBySubstituting(
    unsubstitutedPremiseStatements: Seq[Statement],
    substitutions: Substitutions.Possible,
    availablePremises: Seq[(Statement, Seq[DerivationStep])])(
    implicit stepProvingContext: StepProvingContext
  ): Option[(Seq[DerivationStep], Seq[Statement], Substitutions.Possible)] = {
    def helper(remainingUnsubstitutedPremises: Seq[Statement], currentSubstitutions: Substitutions.Possible, stepsSoFar: Seq[DerivationStep], premisesSoFar: Seq[Statement]): Option[(Seq[DerivationStep], Seq[Statement], Substitutions.Possible)] = {
      remainingUnsubstitutedPremises match {
        case unsubstitutedPremise +: otherUnsubstitutedPremises =>
          (for {
            (stepsForThisPremise, premise, newSubstitutions) <- findPremiseStepsForStatementBySubstituting(unsubstitutedPremise, currentSubstitutions, availablePremises)
            result <- helper(otherUnsubstitutedPremises, newSubstitutions, stepsSoFar ++ stepsForThisPremise, premisesSoFar :+ premise)
          } yield result).headOption
        case Nil =>
          Some((stepsSoFar, premisesSoFar, currentSubstitutions))
      }
    }
    helper(unsubstitutedPremiseStatements, substitutions, Nil, Nil)
  }

  def findPremiseStepsForStatementBySubstituting(
    unsubstitutedPremiseStatement: Statement,
    initialSubstitutions: Substitutions.Possible)(
    implicit stepProvingContext: StepProvingContext
  ): Seq[(Seq[DerivationStep], Statement, Substitutions.Possible)] = {
    unsubstitutedPremiseStatement.applySubstitutions(initialSubstitutions.stripApplications()) match {
      case Some(substitutedPremiseStatement) =>
        for {
          premiseSteps <- findPremiseStepsForStatement(substitutedPremiseStatement).toSeq
        } yield (premiseSteps.deduplicate, substitutedPremiseStatement, initialSubstitutions)
      case None =>
        findPremiseStepsForStatementBySubstituting(unsubstitutedPremiseStatement, initialSubstitutions, stepProvingContext.allPremiseSimplifications)
    }
  }

  def findPremiseStepsForStatementsBySubstituting(
    unsubstitutedPremiseStatements: Seq[Statement],
    substitutions: Substitutions.Possible)(
    implicit stepProvingContext: StepProvingContext
  ): Option[(Seq[DerivationStep], Seq[Statement], Substitutions.Possible)] = {
    def helper(remainingUnsubstitutedPremises: Seq[Statement], currentSubstitutions: Substitutions.Possible, stepsSoFar: Seq[DerivationStep], premisesSoFar: Seq[Statement]): Option[(Seq[DerivationStep], Seq[Statement], Substitutions.Possible)] = {
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
