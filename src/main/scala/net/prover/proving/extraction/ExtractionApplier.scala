package net.prover.proving.extraction

import net.prover.controllers.OptionWithResponseExceptionOps
import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.expressions.{DefinedStatement, Statement, TermVariable}
import net.prover.model.proof.Step.InferenceWithPremiseDerivations
import net.prover.model.proof._
import net.prover.model.unwrapping.Unwrapper
import net.prover.proving.extraction.ExtractionCalculator.{InferenceExtraction, PremiseExtraction, VariableTracker}
import net.prover.proving.premiseFinding.DerivationOrTargetFinder

import scala.util.{Failure, Success, Try}

object ExtractionApplier {
  private case class ExtractionApplication(result: Statement, mainPremise: Statement, extractionSteps: Seq[Step.Assertion], requiredPremises: Seq[Statement])

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
        applySimpleExtractions(extractedConclusion, inferencesRemaining, mainSubstitutions, intendedPremises, intendedConclusion, newVariableTracker)
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
      ExtractionApplication(innerResult, innerPremise, innerSteps, innerPremises) <- applySimpleExtractions(extractedConclusion, inferencesRemaining, mainSubstitutions, innerIntendedPremises, intendedConclusion, variableTracker)
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

  private def applySimpleExtractions(
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

  private def applyExtractions(
    statement: Statement,
    extractionDefinition: ExtractionDefinition,
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement],
    variableTracker: VariableTracker)(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Try[ExtractionApplication] = {
    applySimpleExtractions(statement, extractionDefinition.extractionInferences ++ extractionDefinition.reversalInference.toSeq, substitutions, intendedPremises, intendedConclusion, variableTracker)
  }


  private def applyExtractionsForInference(
    assertionStep: Step.Assertion,
    extractionDefinition: ExtractionDefinition,
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Try[ExtractionApplication] = {
    applyExtractions(assertionStep.statement, extractionDefinition, substitutions, intendedPremises, intendedConclusion, VariableTracker.fromInference(assertionStep.inference))
  }
  private def applyExtractionsForPremise(
    premise: Premise,
    extractionDefinition: ExtractionDefinition,
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Try[ExtractionApplication] = {
    applyExtractions(premise.statement, extractionDefinition, substitutions, intendedPremises, intendedConclusion, VariableTracker.fromStepContext)
  }

  def getInferenceExtractionStepWithoutPremises(
    inferenceExtraction: InferenceExtraction,
    substitutions: Substitutions)(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Option[Step.InferenceApplicationWithoutPremises] = {
    for {
      assertionStep <- Step.Assertion.forInference(inferenceExtraction.inference, substitutions)
      extractionApplication <- ExtractionApplier.applyExtractionsForInference(assertionStep, inferenceExtraction.extractionDefinition, substitutions, None, None).toOption
    } yield createDerivationForInferenceExtraction(assertionStep, extractionApplication.extractionSteps)
  }

  def getInferenceExtractionStepWithPremises(
    inferenceExtraction: InferenceExtraction,
    substitutions: Substitutions,
    unwrappers: Seq[Unwrapper],
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit stepProvingContext: StepProvingContext,
  ): Try[(Step.InferenceApplication, Seq[Step.Target])] = {
    val wrappedStepContext = unwrappers.enhanceStepProvingContext
    for {
      mainAssertion <- Step.Assertion.forInference(inferenceExtraction.inference, substitutions)(wrappedStepContext).orBadRequest("Could not apply substitutions to inference")
      ExtractionApplication(_, mainPremise, extractionSteps, extractionPremises) <-
        ExtractionApplier.applyExtractionsForInference(
          mainAssertion,
          inferenceExtraction.extractionDefinition,
          substitutions,
          intendedPremises,
          intendedConclusion)(
          implicitly,
          wrappedStepContext)
      mainAssertionWithCorrectConclusion = mainAssertion.copy(statement = mainPremise) // mainPremise is equivalent to the existing conclusion here, but with the correct bound variable names
      premises = mainAssertion.premises.map(_.statement) ++ extractionPremises
      extractionStep = createDerivationForInferenceExtraction(mainAssertionWithCorrectConclusion, extractionSteps)
      (wrappedStep, wrappedPremises) = if (unwrappers.nonEmpty) {
        unwrappers.addNecessaryExtractions(extractionStep, premises)
      } else {
        (extractionStep, premises)
      }
      (premiseSteps, targetSteps) = DerivationOrTargetFinder.findDerivationsOrTargets(wrappedPremises)
    } yield (InferenceWithPremiseDerivations.ifNecessary(premiseSteps, wrappedStep), targetSteps)
  }

  def getPremiseExtractionStepWithPremises(
    premise: Premise,
    premiseExtraction: PremiseExtraction,
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Statement, Option[Step], Seq[Step.Target])] = {
    for {
      ExtractionApplication(extractionResult, _, extractionSteps, extractionPremises) <- ExtractionApplier.applyExtractionsForPremise(premise, premiseExtraction.extractionDefinition, substitutions, intendedPremises, intendedConclusion)
      extractionStep = groupStepsByDefinition(extractionSteps) match {
        case Nil => None
        case singleStep +: Nil => Some(singleStep)
        case steps => Some(Step.ExistingStatementExtraction(steps))
      }
      (premiseSteps, targetSteps) = DerivationOrTargetFinder.findDerivationsOrTargets(extractionPremises)
      assertionWithExtractionStep = Step.Elided.ifNecessary(premiseSteps ++ extractionStep.toSeq, "Extracted")
    } yield (extractionResult, assertionWithExtractionStep, targetSteps)
  }

  def createDerivationForInferenceExtraction(
    assertionStep: Step.Assertion,
    extractionSteps: Seq[Step.AssertionOrExtraction])(
    implicit provingContext: ProvingContext
  ): Step.AssertionOrExtraction = {
    assertionStep.addExtractionSteps(groupStepsByDefinition(extractionSteps))
  }

  def groupStepsByDefinition[TStep >: Step.AssertionOrExtraction <: Step.InferenceApplicationWithoutPremises](steps: Seq[TStep])(implicit provingContext: ProvingContext): Seq[TStep] = {
    val deconstructionInferenceIds = provingContext.availableEntries.statementDefinitions.mapCollect(_.deconstructionInference).map(_.id).toSet
    val structuralSimplificationIds = provingContext.structuralSimplificationInferences.map(_._1.id).toSet

    var currentMainStep: Option[Step.Assertion] = None
    val currentUngroupedSteps = Seq.newBuilder[TStep]
    val stepsToReturn = Seq.newBuilder[TStep]

    def isStructuralSimplification(step: TStep): Boolean = {
      structuralSimplificationIds.contains(step.inference.id)
    }

    def removeStructuralSimplifications(steps: Seq[TStep]): Seq[TStep] = {
      steps.filter(s => !isStructuralSimplification(s))
    }

    def removeNonEndStructuralSimplifications(steps: Seq[TStep]): Seq[TStep] = {
      @scala.annotation.tailrec
      def whileStructuralAtEnd(current: Seq[TStep], end: Seq[TStep]): Seq[TStep] = {
        current match {
          case init :+ last if isStructuralSimplification(last) =>
            whileStructuralAtEnd(init, last +: end)
          case _ =>
            removeStructuralSimplifications(current) ++ end
        }
      }

      whileStructuralAtEnd(steps, Nil)
    }

    def groupSteps(steps: Seq[TStep], retainEndSimplifications: Boolean): Unit = {
      currentMainStep match {
        case Some(step) =>
          stepsToReturn += step.addExtractionSteps(removeNonEndStructuralSimplifications(steps))
        case None =>
          stepsToReturn ++= (if (retainEndSimplifications) removeNonEndStructuralSimplifications(steps) else removeStructuralSimplifications(steps))
      }
    }

    for (currentStep <- steps) {
      currentStep match {
        case deconstructionStep@Step.Assertion(_, inference, _, _) if deconstructionInferenceIds.contains(inference.id) =>
          groupSteps(currentUngroupedSteps.result(), false)
          currentMainStep = Some(deconstructionStep)
          currentUngroupedSteps.clear()
        case _ =>
          currentUngroupedSteps += currentStep
      }
    }
    groupSteps(currentUngroupedSteps.result(), true)
    stepsToReturn.result()
  }
}
