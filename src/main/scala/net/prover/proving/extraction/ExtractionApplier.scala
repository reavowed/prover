package net.prover.proving.extraction

import net.prover.controllers.OptionWithResponseExceptionOps
import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.expressions.{DefinedStatement, Statement, TermVariable}
import net.prover.model.proof.Step.InferenceWithPremiseDerivationsStep
import net.prover.model.proof._
import net.prover.model.unwrapping.Unwrapper
import net.prover.proving.derivation.SimpleDerivationStep
import net.prover.proving.premiseFinding.DerivationOrTargetFinder

import scala.util.{Failure, Success, Try}

object ExtractionApplier {
  private case class PartiallyAppliedExtraction(result: Statement, mainPremise: Statement, extractionSteps: Seq[Step.AssertionStep], requiredPremises: Seq[Statement])

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
  ): Try[PartiallyAppliedExtraction] = {
    for {
      predicate <- extractionPremise.calculateSubstitutions(currentStatement).flatMap(_.statements.get(0)).orBadRequest(s"Could not get predicate for specification ${specificationInference.id}")
      boundVariableName <- currentStatement.asOptionalInstanceOf[DefinedStatement].flatMap(_.boundVariableNames.single).orBadRequest(s"Statement $currentStatement did not have a single variable")
      (_, newIndex, newVariableTracker) = variableTracker.getAndAddUniqueVariableName(boundVariableName)
      term <- mainSubstitutions.terms.lift(newIndex).orBadRequest(s"Substitutions did not specify a term at index $newIndex")
      extractionSubstitutions = Substitutions(Seq(predicate), Seq(term))
      extractedConclusion <- specificationInference.conclusion.applySubstitutions(extractionSubstitutions).orBadRequest(s"Could not substitute conclusion for specification ${specificationInference.id}")
      PartiallyAppliedExtraction(innerResult, innerPremise, innerSteps, innerPremises) <-
        applySimpleExtractions(extractedConclusion, inferencesRemaining, mainSubstitutions, intendedPremises, intendedConclusion, newVariableTracker)
      updatedPredicate <- innerPremise.calculateApplicatives(Seq(TermVariable(newIndex, Nil)), Substitutions.Possible(Map.empty, Map(0 -> term)))
        .map(_._1)
        .find(_ == predicate)
        .orBadRequest("Could not recalculate applicative")
      updatedSubstitutions = Substitutions(Seq(updatedPredicate), Seq(term))
      updatedMainPremise <- extractionPremise.applySubstitutions(updatedSubstitutions).orBadRequest("Could not apply updated substitutions")
      updatedMainPremiseWithVariable = updatedMainPremise.asInstanceOf[DefinedStatement].updateBoundVariableNames(Seq(boundVariableName))
      assertionStep = Step.AssertionStep(innerPremise, specificationInference.summary, Seq(Premise.Pending(updatedMainPremiseWithVariable)), updatedSubstitutions)
    } yield PartiallyAppliedExtraction(innerResult, updatedMainPremiseWithVariable, assertionStep +: innerSteps, innerPremises)
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
  ): Try[PartiallyAppliedExtraction] = {
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
      PartiallyAppliedExtraction(innerResult, innerPremise, innerSteps, innerPremises) <- applySimpleExtractions(extractedConclusion, inferencesRemaining, mainSubstitutions, innerIntendedPremises, intendedConclusion, variableTracker)
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
      assertionStep = Step.AssertionStep(innerPremise, inference.summary, (currentStatement +: substitutedPremises).map(Premise.Pending), updatedSubstitutions)
    } yield PartiallyAppliedExtraction(innerResult, updatedMainPremise, assertionStep +: innerSteps, substitutedPremises ++ innerPremises)
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
  ): Try[PartiallyAppliedExtraction] = {
    inferencesRemaining match {
      case inference +: tailInferences =>
        provingContext.specificationInferenceOption match {
          case Some((`inference`, singlePremise)) =>
            applySpecification(currentStatement, inference, singlePremise, variableTracker, tailInferences, substitutions, intendedPremises, intendedConclusion)
          case _ =>
            applySimpleExtraction(currentStatement, inference, variableTracker,  tailInferences, substitutions, intendedPremises, intendedConclusion)
        }
      case Nil =>
        intendedConclusion match {
          case Some(matchingConclusion) if matchingConclusion == currentStatement =>
            Success(PartiallyAppliedExtraction(matchingConclusion, matchingConclusion, Nil, Nil))
          case Some(otherConclusion) =>
            Failure(BadRequestException(s"Intended conclusion '$otherConclusion' did not match expected statement '$currentStatement'"))
          case None =>
            Success(PartiallyAppliedExtraction(currentStatement, currentStatement, Nil, Nil))
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
  ): Try[PartiallyAppliedExtraction] = {
    applySimpleExtractions(statement, extractionDefinition.extractionInferences ++ extractionDefinition.reversalInference.toSeq, substitutions, intendedPremises, intendedConclusion, variableTracker)
  }


  private def applyExtractionsForInference(
    assertionStep: Step.AssertionStep,
    extractionDefinition: ExtractionDefinition,
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Try[PartiallyAppliedExtraction] = {
    applyExtractions(assertionStep.statement, extractionDefinition, substitutions, intendedPremises, intendedConclusion, VariableTracker.fromInference(assertionStep.inference))
  }

  private def applyExtractionsForPremise(
    premise: Premise,
    extractionDefinition: ExtractionDefinition,
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Try[PartiallyAppliedExtraction] = {
    applyExtractions(premise.statement, extractionDefinition, substitutions, intendedPremises, intendedConclusion, VariableTracker.fromStepContext)
  }

  def applyInferenceExtractionWithoutPremises(
    inferenceExtraction: InferenceExtraction,
    substitutions: Substitutions)(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Option[AppliedInferenceExtraction] = {
    for {
      assertionStep <- Step.AssertionStep.forInference(inferenceExtraction.inference, substitutions)
      extractionApplication <- ExtractionApplier.applyExtractionsForInference(assertionStep, inferenceExtraction.extractionDefinition, substitutions, None, None).toOption
    } yield AppliedInferenceExtraction(assertionStep, groupStepsByDefinition(extractionApplication.extractionSteps))
  }

  def getInferenceExtractionStepWithPremises(
    inferenceExtraction: InferenceExtraction,
    substitutions: Substitutions,
    unwrappers: Seq[Unwrapper],
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit stepProvingContext: StepProvingContext,
  ): Try[(Step.InferenceApplication, Seq[Step.TargetStep])] = {
    val wrappedStepContext = unwrappers.enhanceStepProvingContext
    for {
      mainAssertion <- Step.AssertionStep.forInference(inferenceExtraction.inference, substitutions)(wrappedStepContext).orBadRequest("Could not apply substitutions to inference")
      PartiallyAppliedExtraction(_, mainPremise, extractionSteps, extractionPremises) <-
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
      extractionStep = AppliedInferenceExtraction(mainAssertionWithCorrectConclusion, groupStepsByDefinition(extractionSteps)).toStep
      (wrappedStep, wrappedPremises) = if (unwrappers.nonEmpty) {
        unwrappers.addNecessaryExtractions(extractionStep, premises)
      } else {
        (extractionStep, premises)
      }
      (premiseDerivation, targetSteps) = DerivationOrTargetFinder.findDerivationsOrTargets(wrappedPremises)
    } yield (InferenceWithPremiseDerivationsStep.ifNecessary(premiseDerivation.toProofSteps, wrappedStep), targetSteps)
  }

  def getPremiseExtractionStepWithPremises(
    premise: Premise,
    premiseExtraction: PremiseExtraction,
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Statement, Option[Step], Seq[Step.TargetStep])] = {
    for {
      PartiallyAppliedExtraction(extractionResult, _, extractionSteps, extractionPremises) <- ExtractionApplier.applyExtractionsForPremise(premise, premiseExtraction.extractionDefinition, substitutions, intendedPremises, intendedConclusion)
      extraction = groupStepsByDefinition(extractionSteps)
      extractionStep = Step.ExistingStatementExtractionStep.ifNecessary(extraction)
      (premiseDerivation, targetSteps) = DerivationOrTargetFinder.findDerivationsOrTargets(extractionPremises)
      assertionWithExtractionStep = Step.ElidedStep.ifNecessary(premiseDerivation.toProofSteps ++ extractionStep.toSeq, "Extracted")
    } yield (extractionResult, assertionWithExtractionStep, targetSteps)
  }

  def groupStepsByDefinition(steps: Seq[Step.AssertionStep])(implicit provingContext: ProvingContext): AppliedExtraction = {
    val deconstructionInferenceIds = provingContext.availableEntries.statementDefinitions.mapCollect(_.deconstructionInference).map(_.id).toSet
    val structuralSimplificationIds = provingContext.structuralSimplificationInferences.map(_._1.id).toSet

    var currentMainStep: Option[Step.AssertionStep] = None
    val currentUngroupedSteps = Seq.newBuilder[Step.AssertionStep]
    val stepsToReturn = Seq.newBuilder[AppliedExtractionStep]

    def isStructuralSimplification(step: Step.AssertionStep): Boolean = {
      structuralSimplificationIds.contains(step.inference.id)
    }

    def removeStructuralSimplifications(steps: Seq[Step.AssertionStep]): Seq[Step.AssertionStep] = {
      steps.filter(s => !isStructuralSimplification(s))
    }

    def removeNonEndStructuralSimplifications(steps: Seq[Step.AssertionStep]): Seq[Step.AssertionStep] = {
      @scala.annotation.tailrec
      def whileStructuralAtEnd(current: Seq[Step.AssertionStep], end: Seq[Step.AssertionStep]): Seq[Step.AssertionStep] = {
        current match {
          case init :+ last if isStructuralSimplification(last) =>
            whileStructuralAtEnd(init, last +: end)
          case _ =>
            removeStructuralSimplifications(current) ++ end
        }
      }

      whileStructuralAtEnd(steps, Nil)
    }

    def groupSteps(steps: Seq[Step.AssertionStep], retainEndSimplifications: Boolean): Unit = {
      currentMainStep match {
        case Some(step) =>
          stepsToReturn += AppliedExtractionStep.DefinitionDeconstruction(step, removeNonEndStructuralSimplifications(steps))
        case None =>
          stepsToReturn ++= (if (retainEndSimplifications) removeNonEndStructuralSimplifications(steps) else removeStructuralSimplifications(steps))
            .map(AppliedExtractionStep.Assertion)
      }
    }

    for (currentStep <- steps) {
      currentStep match {
        case deconstructionStep@Step.AssertionStep(_, inference, _, _) if deconstructionInferenceIds.contains(inference.id) =>
          groupSteps(currentUngroupedSteps.result(), false)
          currentMainStep = Some(deconstructionStep)
          currentUngroupedSteps.clear()
        case _ =>
          currentUngroupedSteps += currentStep
      }
    }
    groupSteps(currentUngroupedSteps.result(), true)
    AppliedExtraction(stepsToReturn.result())
  }
}
