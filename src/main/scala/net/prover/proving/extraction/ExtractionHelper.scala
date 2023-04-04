package net.prover.proving.extraction

import net.prover.controllers.OptionWithResponseExceptionOps
import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.expressions.{DefinedStatement, Statement, TermVariable}
import net.prover.model.proof._
import net.prover.model.unwrapping.Unwrapper
import net.prover.proving.extraction.SubstatementExtractor.{InferenceExtraction, VariableTracker}
import net.prover.proving.premiseFinding.DerivationOrTargetFinder

import scala.util.{Failure, Success, Try}

object ExtractionHelper {
  private case class ExtractionApplication(result: Statement, mainPremise: Statement, extractionSteps: Seq[Step.InferenceApplicationWithoutPremises], requiredPremises: Seq[Statement])

  private def applySpecification(
    currentStatement: Statement,
    specificationInference: Inference,
    extractionPremise: Statement,
    variableTracker: VariableTracker,
    inferencesRemaining: Seq[Inference],
    mainSubstitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Try[ExtractionApplication] = {
    for {
      predicate <- extractionPremise.calculateSubstitutions(currentStatement).flatMap(_.statements.get(0)).orBadRequest(s"Could not get predicate for specification ${specificationInference.id}")
      boundVariableName <- currentStatement.asOptionalInstanceOf[DefinedStatement].flatMap(_.boundVariableNames.single).orBadRequest(s"Statement $currentStatement did not have a single variable")
      (_, newIndex, newVariableTracker) = variableTracker.getAndAddUniqueVariableName(boundVariableName)
      term <- mainSubstitutions.terms.lift(newIndex).orBadRequest(s"Substitutions did not specify a term at index $newIndex")
      extractionSubstitutions = Substitutions(Seq(predicate), Seq(term))
      extractedConclusion <- specificationInference.conclusion.applySubstitutions(extractionSubstitutions).orBadRequest(s"Could not substitute conclusion for specification ${specificationInference.id}")
      ExtractionApplication(innerResult, innerPremise, innerSteps, innerPremises) <-
        applyExtractions(extractedConclusion, inferencesRemaining, mainSubstitutions, intendedPremises, intendedConclusion, newVariableTracker)
      updatedPredicate <- innerPremise.calculateApplicatives(Seq(TermVariable(newIndex, Nil)), Substitutions.Possible(Map.empty, Map(0 -> term)))
        .map(_._1)
        .find(_ == predicate)
        .orBadRequest("Could not recalculate applicative")
      updatedSubstitutions = Substitutions(Seq(updatedPredicate), Seq(term))
      updatedMainPremise <- extractionPremise.applySubstitutions(updatedSubstitutions).orBadRequest("Could not apply updated substitutions")
      updatedMainPremiseWithVariable = updatedMainPremise.asInstanceOf[DefinedStatement].updateBoundVariableNames(Seq(boundVariableName))
      assertionStep = Step.Assertion(innerPremise, specificationInference.summary, Seq(Premise.Pending(updatedMainPremiseWithVariable)), updatedSubstitutions)
    } yield ExtractionApplication(innerResult, updatedMainPremiseWithVariable, assertionStep +: innerSteps, innerPremises)
  }
  private def applySimpleExtraction(
    currentStatement: Statement,
    inference: Inference,
    variableTracker: VariableTracker,
    inferencesRemaining: Seq[Inference],
    mainSubstitutions: Substitutions,
    intendedPremisesOption: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Try[ExtractionApplication] = {
    for {
      (extractionPremise, otherPremises) <- +:.unapply(inference.premises).filter(_._1.usedVariables.usesAll(inference.variableDefinitions)).orBadRequest(s"Inference ${inference.id} did not have an extraction premise")
      extractionSubstitutions <- extractionPremise.calculateSubstitutions(currentStatement).flatMap(_.confirmTotality(inference.variableDefinitions)).orBadRequest(s"Could not apply extraction premise for inference ${inference.id}")
      extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions).orBadRequest(s"Could not get extraction conclusion for inference ${inference.id}")
      (intendedPremisesForThisInference, innerIntendedPremises) <- intendedPremisesOption match {
        case Some(intendedPremises) =>
          for {
            (intendedPremises, innerIntendedPremises) <- intendedPremises.takeAndRemainingIfValid(otherPremises.length).orBadRequest("Not enough target statements provided")
          } yield (Some(intendedPremises), Some(innerIntendedPremises))
        case None =>
          Success((None, None))
      }
      ExtractionApplication(innerResult, innerPremise, innerSteps, innerPremises) <- applyExtractions(extractedConclusion, inferencesRemaining, mainSubstitutions, innerIntendedPremises, intendedConclusion, variableTracker)
      updatedSubstitutionsFromIntendedPremises <- intendedPremisesForThisInference match {
        case Some(intendedPremises) =>
          for {
            substitutions <- otherPremises.zip(intendedPremises).foldLeft(Try(Substitutions.Possible.empty)) { case (substitutionsSoFarTry, (otherPremise, intendedPremise)) =>
              for {
                substitutionsSoFar <- substitutionsSoFarTry
                newSubstitutions <- otherPremise.calculateSubstitutions(intendedPremise, substitutionsSoFar).orBadRequest(s"Could not calculate substitutions for premise $otherPremise from $intendedPremise")
              } yield newSubstitutions
            }
          } yield substitutions
        case None =>
          Success(Substitutions.Possible.empty)
      }
      updatedSubstitutionsFromInnerPremise <- inference.conclusion.calculateSubstitutions(innerPremise, updatedSubstitutionsFromIntendedPremises).orBadRequest("Could not calculate updated substitutions from inner premise")
      updatedSubstitutions <- extractionPremise.calculateSubstitutions(currentStatement, updatedSubstitutionsFromInnerPremise).flatMap(_.confirmTotality(inference.variableDefinitions)).orBadRequest("Could not calculate updated substitutions from extraction premise")
      updatedMainPremise <- extractionPremise.applySubstitutions(updatedSubstitutions).orBadRequest("Could not apply updated substitutions")
      substitutedPremises <- otherPremises.map(_.applySubstitutions(updatedSubstitutions).orBadRequest(s"Could not apply substitutions to premise")).traverseTry
      assertionStep = Step.Assertion(innerPremise, inference.summary, (currentStatement +: substitutedPremises).map(Premise.Pending), extractionSubstitutions)
    } yield ExtractionApplication(innerResult, updatedMainPremise, assertionStep +: innerSteps, substitutedPremises ++ innerPremises)
  }

  private def applyExtractions(
    currentStatement: Statement,
    inferencesRemaining: Seq[Inference],
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement],
    variableTracker: VariableTracker)(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Try[ExtractionApplication] = {
    inferencesRemaining match {
      case inference +: tailInferences =>
        provingContext.specificationInferenceOption.filter(_._1 == inference)
          .map { case (_, singlePremise) =>
            applySpecification(currentStatement, inference, singlePremise, variableTracker, tailInferences, substitutions, intendedPremises, intendedConclusion)
          } getOrElse applySimpleExtraction(currentStatement, inference, variableTracker,  tailInferences, substitutions, intendedPremises, intendedConclusion)
      case Nil =>
        intendedConclusion match {
          case Some(matchingConclusion) if matchingConclusion == currentStatement =>
            Success(ExtractionApplication(matchingConclusion, matchingConclusion, Nil, Nil))
          case Some(otherConclusion) =>
            Failure(BadRequestException(s"Intended conclusion '$otherConclusion' did not match expected statement '$currentStatement'"))
          case None =>
            Success(ExtractionApplication(currentStatement, currentStatement, Nil, Nil))
        }
    }
  }

  private def groupStepsByDefinition(extractionApplication: ExtractionApplication, initialStep: Option[Step.Assertion])(implicit provingContext: ProvingContext): ExtractionApplication = {
    extractionApplication.copy(extractionSteps = SubstatementExtractor.groupStepsByDefinition(extractionApplication.extractionSteps, initialStep))
  }

  private def applyExtractionsForInference(
    assertionStep: Step.Assertion,
    extractionInferences: Seq[Inference],
    baseInference: Inference,
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Try[ExtractionApplication] = {
    applyExtractions(assertionStep.statement, extractionInferences, substitutions, intendedPremises, intendedConclusion, VariableTracker.fromInference(baseInference))
  }
  private def applyExtractionsForPremise(
    premise: Premise,
    extractionInferences: Seq[Inference],
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit stepContext: StepContext
  ): Try[ExtractionApplication] = {
    applyExtractions(premise.statement, extractionInferences, substitutions, intendedPremises, intendedConclusion, VariableTracker.fromStepContext).map(groupStepsByDefinition(_, None))
  }

  def getInferenceExtractionDerivationWithoutPremises(
    inferenceExtraction: InferenceExtraction,
    substitutions: Substitutions)(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Option[Step.InferenceApplicationWithoutPremises] = {
    for {
      assertionStep <- Step.Assertion.forInference(inferenceExtraction.inference, substitutions)
      extractionApplication <- ExtractionHelper.applyExtractionsForInference(assertionStep, inferenceExtraction.innerExtraction.extractionInferences, inferenceExtraction.inference, substitutions, None, None).toOption
    } yield SubstatementExtractor.createDerivationForInferenceExtraction(assertionStep, extractionApplication.extractionSteps)
  }

  def getInferenceExtractionWithPremises(
    inference: Inference,
    extractionInferences: Seq[Inference],
    substitutions: Substitutions,
    unwrappers: Seq[Unwrapper],
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit stepContext: StepContext,
  ): Try[(Step.InferenceApplication, Seq[Step.Target])] = {
    val wrappedStepContext = unwrappers.enhanceStepContext(stepContext)
    for {
      mainAssertion <- Step.Assertion.forInference(inference, substitutions)(wrappedStepContext).orBadRequest("Could not apply substitutions to inference")
      ExtractionApplication(_, mainPremise, extractionSteps, extractionPremises) <-
        ExtractionHelper.applyExtractionsForInference(
          mainAssertion,
          extractionInferences,
          inference,
          substitutions,
          intendedPremises,
          intendedConclusion)(
          implicitly,
          wrappedStepContext)
      mainAssertionWithCorrectConclusion = mainAssertion.copy(statement = mainPremise) // mainPremise is equivalent to the existing conclusion here, but with the correct bound variable names
      premises = mainAssertion.premises.map(_.statement) ++ extractionPremises
      extractionStep = SubstatementExtractor.createDerivationForInferenceExtraction(mainAssertionWithCorrectConclusion, extractionSteps)
      (wrappedStep, wrappedPremises) = if (unwrappers.nonEmpty) {
        unwrappers.addNecessaryExtractions(extractionStep, premises)
      } else {
        (extractionStep, premises)
      }
      (premiseSteps, targetSteps) = DerivationOrTargetFinder.findDerivationsOrTargets(wrappedPremises)
    } yield (wrappedStep.addPremiseDerivations(premiseSteps), targetSteps)
  }

  def getPremiseExtractionWithPremises(
    premise: Premise,
    extractionInferences: Seq[Inference],
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit stepContext: StepContext
  ): Try[(Statement, Option[Step], Seq[Step.Target])] = {
    for {
      ExtractionApplication(extractionResult, _, extractionSteps, extractionPremises) <- ExtractionHelper.applyExtractionsForPremise(premise, extractionInferences, substitutions, intendedPremises, intendedConclusion)
      extractionStep = extractionSteps match {
        case Nil => None
        case singleStep +: Nil => Some(singleStep)
        case steps => Some(Step.ExistingStatementExtraction(steps))
      }
      (premiseSteps, targetSteps) = DerivationOrTargetFinder.findDerivationsOrTargets(extractionPremises)
      assertionWithExtractionStep = Step.Elided.ifNecessary(premiseSteps ++ extractionStep.toSeq, "Extracted")
    } yield (extractionResult, assertionWithExtractionStep, targetSteps)
  }
}
