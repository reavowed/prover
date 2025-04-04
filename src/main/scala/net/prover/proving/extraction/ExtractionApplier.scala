package net.prover.proving.extraction

import net.prover.controllers.OptionWithResponseExceptionOps
import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.expressions.{DefinedStatement, Statement, TermVariable}
import net.prover.model.proof.Step.InferenceWithPremiseDerivationsStep
import net.prover.model.proof._
import net.prover.model.unwrapping.Unwrapper
import net.prover.proving.derivation.SimpleDerivation
import net.prover.proving.premiseFinding.DerivationOrTargetFinder
import net.prover.proving.structure.inferences.SpecificationInference
import net.prover.proving.structure.statements.BinaryConnectiveStatement

import scala.util.{Failure, Success, Try}

object ExtractionApplier {
  private def applySpecification(
    currentStatement: Statement,
    specificationInference: SpecificationInference,
    variableTracker: VariableTracker,
    innerExtractionDefinition: ExtractionDefinition,
    mainSubstitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Try[PartiallyAppliedExtraction] = {
    for {
      predicate <- specificationInference.premise.calculateSubstitutions(currentStatement)
        .flatMap(_.statements.get(0))
        .orBadRequest(s"Could not get predicate for specification ${specificationInference.inference.id}")
      boundVariableName <- currentStatement.asOptionalInstanceOf[DefinedStatement]
        .flatMap(_.boundVariableNames.single)
        .orBadRequest(s"Statement $currentStatement did not have a single variable")
      (_, newIndex, newVariableTracker) = variableTracker.getAndAddUniqueVariableName(boundVariableName)
      term <- mainSubstitutions.terms.lift(newIndex)
        .orBadRequest(s"Substitutions did not specify a term at index $newIndex")
      extractionSubstitutions = Substitutions(Seq(predicate), Seq(term))
      extractedConclusion <- specificationInference.inference.conclusion.applySubstitutions(extractionSubstitutions)
        .orBadRequest(s"Could not substitute conclusion for specification ${specificationInference.inference.id}")
      innerExtraction <- applyExtraction(extractedConclusion, innerExtractionDefinition, mainSubstitutions, intendedPremises, intendedConclusion, newVariableTracker)
      updatedPredicate <- innerExtraction.mainPremise.calculateApplicatives(Seq(TermVariable(newIndex, Nil)), Substitutions.Possible(Map.empty, Map(0 -> term)))
        .map(_._1)
        .find(_ == predicate)
        .orBadRequest("Could not recalculate applicative")
      updatedSubstitutions = Substitutions(Seq(updatedPredicate), Seq(term))
      updatedMainPremise <- specificationInference.premise.applySubstitutions(updatedSubstitutions)
        .orBadRequest("Could not apply updated substitutions")
      updatedMainPremiseWithVariable = updatedMainPremise.asInstanceOf[DefinedStatement].updateBoundVariableNames(Seq(boundVariableName))
      assertionStep = Step.AssertionStep(innerExtraction.mainPremise, specificationInference.inference.summary, Seq(Premise.Pending(updatedMainPremiseWithVariable)), updatedSubstitutions)
    } yield innerExtraction.prependExtractionStep(assertionStep)
  }

  private def applySimpleExtraction(
    currentStatement: Statement,
    inference: Inference,
    variableTracker: VariableTracker,
    innerExtractionDefinition: ExtractionDefinition,
    updateExtraction: (PartiallyAppliedExtraction, Step.AssertionStep) => PartiallyAppliedExtraction,
    mainSubstitutions: Substitutions,
    intendedPremisesOption: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Try[PartiallyAppliedExtraction] = {
    for {
      (extractionPremise, otherPremises) <- +:.unapply(inference.premises)
        .filter(_._1.usedVariables.usesAll(inference.variableDefinitions))
        .orBadRequest(s"Inference ${inference.id} did not have an extraction premise")
      extractionSubstitutions <- extractionPremise.calculateSubstitutions(currentStatement)
        .flatMap(_.confirmTotality(inference.variableDefinitions))
        .orBadRequest(s"Could not apply extraction premise for inference ${inference.id}")
      extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions)
        .orBadRequest(s"Could not get extraction conclusion for inference ${inference.id}")
      (intendedPremisesForThisInference, innerIntendedPremises) <- intendedPremisesOption match {
        case Some(intendedPremises) =>
          for {
            (intendedPremises, innerIntendedPremises) <- intendedPremises.takeAndRemainingIfValid(otherPremises.length)
              .orBadRequest("Not enough target statements provided")
          } yield (Some(intendedPremises), Some(innerIntendedPremises))
        case None =>
          Success((None, None))
      }
      innerExtraction <- applyExtraction(extractedConclusion, innerExtractionDefinition, mainSubstitutions, innerIntendedPremises, intendedConclusion, variableTracker)
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
      updatedSubstitutionsFromInnerPremise <- inference.conclusion.calculateSubstitutions(innerExtraction.mainPremise, updatedSubstitutionsFromIntendedPremises).orBadRequest("Could not calculate updated substitutions from inner premise")
      updatedSubstitutions <- extractionPremise.calculateSubstitutions(currentStatement, updatedSubstitutionsFromInnerPremise).flatMap(_.confirmTotality(inference.variableDefinitions)).orBadRequest("Could not calculate updated substitutions from extraction premise")
      updatedMainPremise <- extractionPremise.applySubstitutions(updatedSubstitutions).orBadRequest("Could not apply updated substitutions")
      substitutedPremises <- otherPremises.map(_.applySubstitutions(updatedSubstitutions).orBadRequest(s"Could not apply substitutions to premise")).traverseTry
      assertionStep = Step.AssertionStep(innerExtraction.mainPremise, inference.summary, (updatedMainPremise +: substitutedPremises).map(Premise.Pending), updatedSubstitutions)
    } yield updateExtraction(innerExtraction, assertionStep)
  }

  private def applyLeftRewrite(
    currentStatement: Statement,
    inference: Inference,
    variableTracker: VariableTracker,
    innerExtractionDefinition: ExtractionDefinition,
    mainSubstitutions: Substitutions,
    intendedPremisesOption: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Try[PartiallyAppliedExtraction] = {
    for {
      BinaryConnectiveStatement(connective, left, right) <- provingContext.asBinaryConnectiveStatement(currentStatement)
        .orBadRequest(s"Statement $currentStatement not a connective")
      rewrite <- provingContext.chainableRewriteInferences.find(_.inference == inference)
        .orBadRequest(s"Could not find chainable rewrite with inference ${inference.id}")
      transitivity <- rewrite.findValidTransitivity(connective)
        .orBadRequest("Could not find transitivity")
      rewriteSubstitutions <- rewrite.right.calculateSubstitutions(left)
        .flatMap(_.confirmTotality(rewrite.inference.variableDefinitions))
        .orBadRequest("Could not calculate rewrite substitutions")
      rewrittenLeft <- rewrite.left.applySubstitutions(rewriteSubstitutions)
        .orBadRequest("Could not apply rewrite substitutions")
      rewrittenStatement = connective(rewrittenLeft, right)
      innerExtraction <- applyExtraction(rewrittenStatement, innerExtractionDefinition, mainSubstitutions, intendedPremisesOption, intendedConclusion, variableTracker)
      (updatedRewrittenLeft, updatedRight) <- connective.unapply(innerExtraction.mainPremise)
        .orBadRequest("Inner extraction main premise did not match connective")
      updatedSubstitutions <- rewrite.left.calculateSubstitutions(updatedRewrittenLeft)
        .flatMap(_.confirmTotality(rewrite.inference.variableDefinitions))
        .orBadRequest("Could not calculate updated substitutions from inner premise")
      updatedLeft <- rewrite.right.applySubstitutions(updatedSubstitutions)
        .orBadRequest("Could not apply substitutions to rewrite premise ")
      rewriteStep = Step.AssertionStep(connective(updatedRewrittenLeft, updatedLeft), rewrite.inference.summary, Nil, updatedSubstitutions)
      transitivityStep = transitivity.assertionStep(updatedRewrittenLeft, updatedLeft, updatedRight)
    } yield innerExtraction.prependLeftRewrite(rewriteStep, transitivityStep)
  }

  private def applyRightRewrite(
    currentStatement: Statement,
    inference: Inference,
    variableTracker: VariableTracker,
    innerExtractionDefinition: ExtractionDefinition,
    mainSubstitutions: Substitutions,
    intendedPremisesOption: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Try[PartiallyAppliedExtraction] = {
    for {
      BinaryConnectiveStatement(connective, left, right) <- provingContext.asBinaryConnectiveStatement(currentStatement)
        .orBadRequest(s"Statement $currentStatement not a connective")
      rewrite <- provingContext.chainableRewriteInferences.find(_.inference == inference)
        .orBadRequest(s"Could not find chainable rewrite with inference ${inference.id}")
      transitivity <- rewrite.findValidTransitivity(connective)
        .orBadRequest("Could not find transitivity")
      rewriteSubstitutions <- rewrite.left.calculateSubstitutions(right)
        .flatMap(_.confirmTotality(rewrite.inference.variableDefinitions))
        .orBadRequest("Could not calculate rewrite substitutions")
      rewrittenRight <- rewrite.right.applySubstitutions(rewriteSubstitutions)
        .orBadRequest("Could not apply rewrite substitutions")
      rewrittenStatement = connective(left, rewrittenRight)
      innerExtraction <- applyExtraction(rewrittenStatement, innerExtractionDefinition, mainSubstitutions, intendedPremisesOption, intendedConclusion, variableTracker)
      (updatedLeft, updatedRewrittenRight) <- connective.unapply(innerExtraction.mainPremise)
        .orBadRequest("Inner extraction main premise did not match connective")
      updatedSubstitutions <- rewrite.right.calculateSubstitutions(updatedRewrittenRight)
        .flatMap(_.confirmTotality(rewrite.inference.variableDefinitions))
        .orBadRequest("Could not calculate updated substitutions from inner premise")
      updatedRight <- rewrite.left.applySubstitutions(updatedSubstitutions)
        .orBadRequest("Could not apply substitutions to rewrite premise ")
      rewriteStep = Step.AssertionStep(connective(updatedRight, updatedRewrittenRight), rewrite.inference.summary, Nil, updatedSubstitutions)
      transitivityStep = transitivity.assertionStep(updatedLeft, updatedRight, updatedRewrittenRight)
    } yield innerExtraction.prependRightRewrite(rewriteStep, transitivityStep)
  }

  private def applyExtraction(
    currentStatement: Statement,
    extractionDefinition: ExtractionDefinition,
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement],
    variableTracker: VariableTracker)(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Try[PartiallyAppliedExtraction] = {
    extractionDefinition.extractionInferences match {
      case inference +: tailInferences =>
        provingContext.specificationInferenceOption match {
          case Some(specificationInference) if specificationInference.inference == inference =>
            applySpecification(
              currentStatement,
              specificationInference,
              variableTracker,
              extractionDefinition.copy(extractionInferences = tailInferences),
              substitutions,
              intendedPremises,
              intendedConclusion)
          case _ =>
            applySimpleExtraction(
              currentStatement,
              inference,
              variableTracker,
              extractionDefinition.copy(extractionInferences = tailInferences),
              _.prependExtractionStep(_),
              substitutions,
              intendedPremises,
              intendedConclusion)
        }
      case Nil =>
        extractionDefinition.reversalInference match {
          case Some(inference) =>
            applySimpleExtraction(
              currentStatement,
              inference,
              variableTracker,
              extractionDefinition.copy(reversalInference = None),
              _.prependReversal(_),
              substitutions,
              intendedPremises,
              intendedConclusion)
          case None =>
            extractionDefinition.leftRewrite match {
              case Some(inference) =>
                applyLeftRewrite(
                  currentStatement,
                  inference,
                  variableTracker,
                  extractionDefinition.copy(leftRewrite = None),
                  substitutions,
                  intendedPremises,
                  intendedConclusion)
              case None =>
                extractionDefinition.rightRewrite match {
                  case Some(inference) =>
                    applyRightRewrite(
                      currentStatement,
                      inference,
                      variableTracker,
                      extractionDefinition.copy(rightRewrite = None),
                      substitutions,
                      intendedPremises,
                      intendedConclusion)
                  case None =>
                    intendedConclusion match {
                      case Some(matchingConclusion) if matchingConclusion == currentStatement =>
                        Success(PartiallyAppliedExtraction.initial(matchingConclusion, variableTracker))
                      case Some(otherConclusion) =>
                        Failure(BadRequestException(s"Intended conclusion '$otherConclusion' did not match expected statement '$currentStatement'"))
                      case None =>
                        Success(PartiallyAppliedExtraction.initial(currentStatement, variableTracker))
                    }
                }
            }
        }
    }
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
    applyExtraction(assertionStep.statement, extractionDefinition, substitutions, intendedPremises, intendedConclusion, VariableTracker.fromInference(assertionStep.inference))
  }

  private def applyExtractionsForPremise(
    premise: Premise,
    extractionDefinition: ExtractionDefinition,
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement])(
    implicit stepProvingContext: StepProvingContext
  ): Try[PartiallyAppliedExtraction] = {
    applyExtraction(premise.statement, extractionDefinition, substitutions, intendedPremises, intendedConclusion, VariableTracker.fromStepContext)
  }

  def applyInferenceExtractionWithoutPremises(
    inferenceExtraction: InferenceExtraction,
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]] = None,
    intendedConclusion: Option[Statement] = None)(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Option[(AppliedInferenceExtraction, Seq[Statement])] = {
    for {
      assertionStep <- Step.AssertionStep.forInference(inferenceExtraction.inference, substitutions)
      extraction <- ExtractionApplier.applyExtractionsForInference(
        assertionStep,
        inferenceExtraction.extractionDefinition,
        substitutions,
        intendedPremises,
        intendedConclusion
      ).toOption
      assertionStepWithCorrectConclusion = assertionStep.copy(statement = extraction.mainPremise) // mainPremise is equivalent to the existing conclusion here, but with the correct bound variable names
      appliedExtraction = AppliedInferenceExtraction(assertionStepWithCorrectConclusion, extraction.finalise)
      premises = assertionStepWithCorrectConclusion.premises.map(_.statement) ++ extraction.extractionPremises
    } yield (appliedExtraction, premises)
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
      (appliedExtraction, premises) <- applyInferenceExtractionWithoutPremises(
        inferenceExtraction,
        substitutions,
        intendedPremises,
        intendedConclusion)(
        implicitly,
        wrappedStepContext
      ).orBadRequest("Could not apply extraction")
      appliedExtractionStep = appliedExtraction.toStep
      (wrappedExtractionStep, wrappedPremises) = if (unwrappers.nonEmpty) {
        unwrappers.addNecessaryExtractions(appliedExtractionStep, premises)
      } else {
        (appliedExtractionStep, premises)
      }
      (knownStatements, targetSteps) = DerivationOrTargetFinder.findDerivationsOrTargets(wrappedPremises)
    } yield (InferenceWithPremiseDerivationsStep.ifNecessary(knownStatements, wrappedExtractionStep), targetSteps)
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
      extraction <- ExtractionApplier.applyExtractionsForPremise(premise, premiseExtraction.extractionDefinition, substitutions, intendedPremises, intendedConclusion)
      (knownStatements, targetSteps) = DerivationOrTargetFinder.findDerivationsOrTargets(extraction.extractionPremises)
      extractionStep = Step.ExistingStatementExtractionStep.ifNecessary(knownStatements, extraction.finalise)
    } yield (extraction.conclusion, extractionStep, targetSteps)
  }

  def groupStepsByDefinition(steps: Seq[Step.AssertionStep])(implicit provingContext: ProvingContext): Seq[AppliedExtractionStep] = {
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
    stepsToReturn.result()
  }
}
