package net.prover.controllers

import net.prover.exceptions.BadRequestException
import net.prover._
import net.prover.model._
import net.prover.model.expressions.{DefinedStatement, Statement, TermVariable}
import net.prover.model.proof.SubstatementExtractor.{InferenceExtraction, VariableTracker}
import net.prover.model.proof._
import net.prover.old.OldSubstitutionApplier
import net.prover.substitutionFinding.model.PossibleSubstitutions
import net.prover.substitutionFinding.transformers.PossibleSubstitutionCalculator

import scala.util.{Failure, Success, Try}

object ExtractionHelper {
  case class ExtractionApplication(result: Statement, mainPremise: Statement, extractionSteps: Seq[DerivationStep], premiseSteps: Seq[DerivationStep], targetSteps: Seq[Step.Target])

  private def applySpecification(
    currentStatement: Statement,
    specificationInference: Inference,
    extractionPremise: Statement,
    variableTracker: VariableTracker,
    inferencesRemaining: Seq[Inference],
    mainSubstitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement],
    findPremiseStepsOrTargets: Seq[Statement] => (Seq[DerivationStep], Seq[Step.Target]))(
    implicit stepProvingContext: StepProvingContext
  ): Try[ExtractionApplication] = {
    for {
      predicate <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(extractionPremise, currentStatement).flatMap(_.statements.get(0)).orBadRequest(s"Could not get predicate for specification ${specificationInference.id}")
      boundVariableName <- currentStatement.asOptionalInstanceOf[DefinedStatement].flatMap(_.boundVariableNames.single).orBadRequest(s"Statement $currentStatement did not have a single variable")
      (_, newIndex, newVariableTracker) = variableTracker.getAndAddUniqueVariableName(boundVariableName)
      term <- mainSubstitutions.terms.lift(newIndex).orBadRequest(s"Substitutions did not specify a term at index $newIndex")
      extractionSubstitutions = Substitutions(Seq(predicate), Seq(term))
      extractedConclusion <- OldSubstitutionApplier.applySubstitutions(specificationInference.conclusion, extractionSubstitutions).orBadRequest(s"Could not substitute conclusion for specification ${specificationInference.id}")
      ExtractionApplication(innerResult, innerPremise, innerSteps, innerPremises, innerTargets) <-
        applyExtractions(extractedConclusion, inferencesRemaining, mainSubstitutions, intendedPremises, intendedConclusion, newVariableTracker, findPremiseStepsOrTargets)
      updatedPredicate <- innerPremise.calculateApplicatives(Seq(TermVariable(newIndex, Nil)), PossibleSubstitutions(Map.empty, Map(0 -> term)))
        .map(_._1)
        .find(_ == predicate)
        .orBadRequest("Could not recalculate applicative")
      updatedSubstitutions = Substitutions(Seq(updatedPredicate), Seq(term))
      updatedMainPremise <- OldSubstitutionApplier.applySubstitutions(extractionPremise, updatedSubstitutions).orBadRequest("Could not apply updated substitutions")
      updatedMainPremiseWithVariable = updatedMainPremise.asInstanceOf[DefinedStatement].updateBoundVariableNames(Seq(boundVariableName))
      assertionStep = Step.Assertion(innerPremise, specificationInference.summary, Seq(Premise.Pending(updatedMainPremiseWithVariable)), updatedSubstitutions)
    } yield ExtractionApplication(innerResult, updatedMainPremiseWithVariable, DerivationStep.fromAssertion(assertionStep) +: innerSteps, innerPremises, innerTargets)
  }
  private def applySimpleExtraction(
    currentStatement: Statement,
    inference: Inference,
    variableTracker: VariableTracker,
    inferencesRemaining: Seq[Inference],
    mainSubstitutions: Substitutions,
    intendedPremisesOption: Option[Seq[Statement]],
    intendedConclusion: Option[Statement],
    findPremiseStepsOrTargets: Seq[Statement] => (Seq[DerivationStep], Seq[Step.Target]))(
    implicit stepProvingContext: StepProvingContext
  ): Try[ExtractionApplication] = {
    for {
      (extractionPremise, otherPremises) <- +:.unapply(inference.premises).filter(_._1.usedVariables.usesAll(inference.variableDefinitions)).orBadRequest(s"Inference ${inference.id} did not have an extraction premise")
      extractionSubstitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(extractionPremise, currentStatement).flatMap(_.confirmTotality(inference.variableDefinitions)).orBadRequest(s"Could not apply extraction premise for inference ${inference.id}")
      extractedConclusion <- OldSubstitutionApplier.applySubstitutions(inference.conclusion, extractionSubstitutions).orBadRequest(s"Could not get extraction conclusion for inference ${inference.id}")
      (intendedPremisesForThisInference, innerIntendedPremises) <- intendedPremisesOption match {
        case Some(intendedPremises) =>
          for {
            (intendedPremises, innerIntendedPremises) <- intendedPremises.takeAndRemainingIfValid(otherPremises.length).orBadRequest("Not enough target statements provided")
          } yield (Some(intendedPremises), Some(innerIntendedPremises))
        case None =>
          Success((None, None))
      }
      ExtractionApplication(innerResult, innerPremise, innerSteps, innerPremises, innerTargets) <- applyExtractions(extractedConclusion, inferencesRemaining, mainSubstitutions, innerIntendedPremises, intendedConclusion, variableTracker, findPremiseStepsOrTargets)
      updatedSubstitutionsFromIntendedPremises <- intendedPremisesForThisInference match {
        case Some(intendedPremises) =>
          for {
            substitutions <- otherPremises.zip(intendedPremises).foldLeft(Try(PossibleSubstitutions.empty)) { case (substitutionsSoFarTry, (otherPremise, intendedPremise)) =>
              for {
                substitutionsSoFar <- substitutionsSoFarTry
                newSubstitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(otherPremise, intendedPremise, substitutionsSoFar).orBadRequest(s"Could not calculate substitutions for premise $otherPremise from $intendedPremise")
              } yield newSubstitutions
            }
          } yield substitutions
        case None =>
          Success(PossibleSubstitutions.empty)
      }
      updatedSubstitutionsFromInnerPremise <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(inference.conclusion, innerPremise, updatedSubstitutionsFromIntendedPremises).orBadRequest("Could not calculate updated substitutions from inner premise")
      updatedSubstitutions <- PossibleSubstitutionCalculator.calculatePossibleSubstitutions(extractionPremise, currentStatement, updatedSubstitutionsFromInnerPremise).flatMap(_.confirmTotality(inference.variableDefinitions)).orBadRequest("Could not calculate updated substitutions from extraction premise")
      updatedMainPremise <- OldSubstitutionApplier.applySubstitutions(extractionPremise, updatedSubstitutions).orBadRequest("Could not apply updated substitutions")
      substitutedPremises <- otherPremises.map(OldSubstitutionApplier.applySubstitutions(_, updatedSubstitutions).orBadRequest(s"Could not apply substitutions to premise")).traverseTry
      (premiseSteps, targetSteps) = findPremiseStepsOrTargets(substitutedPremises)
      assertionStep = Step.Assertion(innerPremise, inference.summary, (currentStatement +: substitutedPremises).map(Premise.Pending), extractionSubstitutions)
    } yield ExtractionApplication(innerResult, updatedMainPremise, DerivationStep.fromAssertion(assertionStep) +: innerSteps, premiseSteps ++ innerPremises, targetSteps ++ innerTargets)
  }

  private def applyExtractions(
    currentStatement: Statement,
    inferencesRemaining: Seq[Inference],
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement],
    variableTracker: VariableTracker,
    findPremiseStepsOrTargets: Seq[Statement] => (Seq[DerivationStep], Seq[Step.Target]))(
    implicit stepProvingContext: StepProvingContext
  ): Try[ExtractionApplication] = {
    inferencesRemaining match {
      case inference +: tailInferences =>
        stepProvingContext.provingContext.specificationInferenceOption.filter(_._1 == inference)
          .map { case (_, singlePremise) =>
            applySpecification(currentStatement, inference, singlePremise, variableTracker, tailInferences, substitutions, intendedPremises, intendedConclusion, findPremiseStepsOrTargets)
          } getOrElse applySimpleExtraction(currentStatement, inference, variableTracker,  tailInferences, substitutions, intendedPremises, intendedConclusion, findPremiseStepsOrTargets)
      case Nil =>
        intendedConclusion match {
          case Some(matchingConclusion) if matchingConclusion == currentStatement =>
            Success(ExtractionApplication(matchingConclusion, matchingConclusion, Nil, Nil, Nil))
          case Some(otherConclusion) =>
            Failure(BadRequestException(s"Intended conclusion '$otherConclusion' did not match expected statement '$currentStatement'"))
          case None =>
            Success(ExtractionApplication(currentStatement, currentStatement, Nil, Nil, Nil))
        }
    }
  }

  private def groupStepsByDefinition(extractionApplication: ExtractionApplication, initialStep: Option[DerivationStepWithSingleInference])(implicit provingContext: ProvingContext): ExtractionApplication = {
    extractionApplication.copy(extractionSteps = SubstatementExtractor.groupStepsByDefinition(extractionApplication.extractionSteps, initialStep))
  }

  def applyExtractionsForInference(
    assertionStep: Step.Assertion,
    extractionInferences: Seq[Inference],
    baseInference: Inference,
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement],
    findPremiseStepsOrTargets: Seq[Statement] => (Seq[DerivationStep], Seq[Step.Target]))(
    implicit stepProvingContext: StepProvingContext
  ): Try[ExtractionApplication] = {
    applyExtractions(assertionStep.statement, extractionInferences, substitutions, intendedPremises, intendedConclusion, VariableTracker.fromInference(baseInference), findPremiseStepsOrTargets)
  }
  def applyExtractionsForPremise(
    premise: Premise,
    extractionInferences: Seq[Inference],
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement],
    findPremiseStepsOrTargets: Seq[Statement] => (Seq[DerivationStep], Seq[Step.Target]))(
    implicit stepProvingContext: StepProvingContext
  ): Try[ExtractionApplication] = {
    applyExtractions(premise.statement, extractionInferences, substitutions, intendedPremises, intendedConclusion, VariableTracker.fromStepContext, findPremiseStepsOrTargets).map(groupStepsByDefinition(_, None))
  }

  def getInferenceExtractionDerivationWithoutPremises(
    inferenceExtraction: InferenceExtraction,
    substitutions: Substitutions)(
    implicit stepProvingContext: StepProvingContext
  ): Option[DerivationStep] = {
    for {
      assertionStep <- Step.Assertion.forInference(inferenceExtraction.inference, substitutions)
      extractionApplication <- ExtractionHelper.applyExtractionsForInference(assertionStep, inferenceExtraction.innerExtraction.extractionInferences, inferenceExtraction.inference, substitutions, None, None, _ => (Nil, Nil)).toOption
    } yield SubstatementExtractor.createDerivationForInferenceExtraction(assertionStep, extractionApplication.extractionSteps)
  }

  def getInferenceExtractionWithPremises(
    inference: Inference,
    extractionInferences: Seq[Inference],
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Try[(DerivationStep, Seq[Step.Target])] = {
    for {
      (mainAssertion, mainPremises, mainTargets) <- ProofHelper.getAssertionWithPremises(inference, substitutions).orBadRequest("Could not apply substitutions to inference")
      ExtractionApplication(_, mainPremise, extractionSteps, extractionPremises, extractionTargets) <- ExtractionHelper.applyExtractionsForInference(mainAssertion, extractionInferences, inference, substitutions, intendedPremises, intendedConclusion, PremiseFinder.findDerivationsOrTargets)
      mainAssertionWithCorrectConclusion = mainAssertion.copy(statement = mainPremise)
      extractionStep = SubstatementExtractor.createDerivationForInferenceExtraction(mainAssertionWithCorrectConclusion, extractionSteps)
    } yield (extractionStep.elideWithPremiseSteps((mainPremises ++ extractionPremises).deduplicate), mainTargets ++ extractionTargets)
  }

  def getPremiseExtractionWithoutPremises(
    premise: Premise,
    extractionInferences: Seq[Inference])(
    implicit stepProvingContext: StepProvingContext
  ): Option[(Statement, Seq[DerivationStep])] = {
    for {
      extractionApplication <- applyExtractionsForPremise(premise, extractionInferences, Substitutions.empty, None, None, s => (Nil, s.map(Step.Target(_)))).toOption
    } yield (extractionApplication.result, extractionApplication.extractionSteps)
  }

  def getPremiseExtractionWithPremises(
    premise: Premise,
    extractionInferences: Seq[Inference],
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Statement, Option[Step], Seq[Step.Target])] = {
    for {
      ExtractionApplication(extractionResult, _, extractionSteps, extractionPremises, extractionTargets) <- ExtractionHelper.applyExtractionsForPremise(premise, extractionInferences, substitutions, intendedPremises, intendedConclusion, PremiseFinder.findDerivationsOrTargets)
      extractionStep = Step.Elided.ifNecessary(extractionSteps.steps, "Extracted")
      assertionWithExtractionStep = Step.Elided.ifNecessary(extractionPremises.deduplicate.steps ++ extractionStep.toSeq, "Extracted")
    } yield (extractionResult, assertionWithExtractionStep, extractionTargets)
  }
}
