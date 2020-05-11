package net.prover.controllers

import net.prover.exceptions.BadRequestException
import net.prover.model.{definitions, _}
import net.prover.model.expressions.{DefinedStatement, Statement, TermVariable}
import net.prover.model.proof.SubstatementExtractor.{ExtractionOption, VariableTracker}
import net.prover.model.proof.{DerivationStep, Premise, PremiseFinder, ProofHelper, Step, StepProvingContext}

import scala.util.{Failure, Success, Try}

object ExtractionHelper {
  case class ExtractionApplication(result: Statement, mainPremise: Statement, extractionSteps: Seq[DerivationStep], premiseSteps: Seq[DerivationStep], targetSteps: Seq[Step.Target])

  private def applySpecification(
    currentStatement: Statement,
    specificationInference: Inference,
    extractionPremise: Statement,
    predicateName: String,
    variableName: String,
    variableTracker: VariableTracker,
    inferencesRemaining: Seq[Inference],
    mainSubstitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement],
    findPremiseStepsOrTargets: Seq[Statement] => (Seq[DerivationStep], Seq[Step.Target]))(
    implicit stepProvingContext: StepProvingContext
  ): Try[ExtractionApplication] = {
    for {
      extractionSubstitutionsWithoutVariable <- extractionPremise.calculateSubstitutions(currentStatement).flatMap(_.confirmTotality).orBadRequest(s"Could not apply extraction premise for inference ${specificationInference.id}")
      boundVariableName <- currentStatement.asOptionalInstanceOf[DefinedStatement].flatMap(_.boundVariableNames.single).orBadRequest(s"Statement $currentStatement did not have a single variable")
      (newName, newVariableTracker) = variableTracker.getAndAddUniqueVariableName(boundVariableName)
      term <- mainSubstitutions.terms.get(newName).orBadRequest(s"Substitutions did not specify a term '$newName'")
      extractionSubstitutions = extractionSubstitutionsWithoutVariable.copy(terms = extractionSubstitutionsWithoutVariable.terms + (variableName -> term))
      extractedConclusion <- specificationInference.conclusion.applySubstitutions(extractionSubstitutions).orBadRequest(s"Could not get extraction conclusion for inference ${specificationInference.id}")
      ExtractionApplication(innerResult, innerPremise, innerSteps, innerPremises, innerTargets) <-
        applyExtractions(extractedConclusion, inferencesRemaining, mainSubstitutions, intendedPremises, intendedConclusion, newVariableTracker, findPremiseStepsOrTargets)
      predicate = extractionSubstitutions.statements(predicateName)._2
      updatedPredicate <- innerPremise.calculateApplicatives(Seq(TermVariable(variableName, Nil)), Substitutions(terms = Map(variableName -> term)))
        .map(_._1).find(_ == predicate)
        .orBadRequest("Could not recalculate applicative")
      updatedSubstitutions = Substitutions(Map(predicateName -> (1, updatedPredicate)), Map(variableName -> term))
      updatedMainPremise <- extractionPremise.applySubstitutions(updatedSubstitutions).orBadRequest("Could not apply updated substitutions")
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
      (extractionPremise, otherPremises) <- +:.unapply(inference.premises).filter(_._1.requiredSubstitutions.contains(inference.requiredSubstitutions)).orBadRequest(s"Inference ${inference.id} did not have an extraction premise")
      extractionSubstitutions <- extractionPremise.calculateSubstitutions(currentStatement).flatMap(_.confirmTotality).orBadRequest(s"Could not apply extraction premise for inference ${inference.id}")
      extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions).orBadRequest(s"Could not get extraction conclusion for inference ${inference.id}")
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
      updatedSubstitutions <- extractionPremise.calculateSubstitutions(currentStatement, updatedSubstitutionsFromInnerPremise).flatMap(_.confirmTotality).orBadRequest("Could not calculate updated substitutions from extraction premise")
      updatedMainPremise <- extractionPremise.applySubstitutions(updatedSubstitutions).orBadRequest("Could not apply updated substitutions")
      substitutedPremises <- otherPremises.map(_.applySubstitutions(updatedSubstitutions).orBadRequest(s"Could not apply substitutions to premise")).traverseTry
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
          .map { case (_, singlePremise, predicateName, variableName) =>
            applySpecification(currentStatement, inference, singlePremise, predicateName, variableName, variableTracker, tailInferences, substitutions, intendedPremises, intendedConclusion, findPremiseStepsOrTargets)
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

  private def groupStepsByDefinition(extractionApplication: ExtractionApplication, initialStep: Option[DerivationStep])(implicit provingContext: ProvingContext): ExtractionApplication = {
    extractionApplication.copy(extractionSteps = groupStepsByDefinition(extractionApplication.extractionSteps, initialStep))
  }
  private def groupStepsByDefinition(steps: Seq[DerivationStep], initialMainStep: Option[DerivationStep])(implicit provingContext: ProvingContext): Seq[DerivationStep] = {
    val structuralSimplificationIds = provingContext.structuralSimplificationInferences.map(_._1.id).toSet

    var currentMainStep: Option[DerivationStep] = initialMainStep
    val currentUngroupedSteps = Seq.newBuilder[DerivationStep]
    val stepsToReturn = Seq.newBuilder[DerivationStep]

    def removeUnnecessarySimplifications(steps: Seq[DerivationStep], typeSymbol: String): Seq[DerivationStep] = {
      val definitions = provingContext.entryContext.typeStatementDefinitionsByType(typeSymbol)
      def isTypeStatement(statement: Statement): Boolean = {
        statement.asOptionalInstanceOf[DefinedStatement].map(_.definition).contains(provingContext.entryContext.typeDefinitions(typeSymbol).statementDefinition)
      }
      def isAccompanyingStatement(statement: Statement): Boolean = {
        statement.asOptionalInstanceOf[DefinedStatement].map(_.definition).exists(definitions.contains)
      }
      def isCombinedTypeStatement(statement: Statement): Boolean = {
        isTypeStatement(statement) ||
          provingContext.entryContext.conjunctionDefinitionOption.exists { conjunction =>
            conjunction.unapply(statement).exists { case (a, b) => isCombinedTypeStatement(a) && isAccompanyingStatement(b) }
          }
      }
      @scala.annotation.tailrec
      def helper(cleared: Seq[DerivationStep], remaining: Seq[DerivationStep]): Seq[DerivationStep] = {
        remaining match {
          case Nil =>
            cleared
          case head +: tail =>
            if (isCombinedTypeStatement(head.statement) && tail.forall(s => structuralSimplificationIds.contains(s.inference.id)))
              cleared :+ head
            else
              helper(cleared :+ head, tail)
        }
      }
      helper(Nil, steps)
    }
    def removeNonEndStructuralSimplifications(steps: Seq[DerivationStep]): Seq[DerivationStep] = {
      @scala.annotation.tailrec
      def helper(remainingAssertions: Seq[DerivationStep], filteredAssertions: Seq[DerivationStep]): Seq[DerivationStep] = {
        remainingAssertions match {
          case head +: tail if structuralSimplificationIds.contains(head.inference.id) && tail.exists(a => !structuralSimplificationIds.contains(a.inference.id)) =>
            helper(tail, filteredAssertions)
          case head +: tail =>
            helper(tail, filteredAssertions :+ head)
          case Nil =>
            filteredAssertions
        }
      }
      helper(steps, Nil)
    }
    def groupSteps(steps: Seq[DerivationStep]): Unit = {
      currentMainStep match {
        case Some(step) =>
          val elidedStep = Step.Elided.ifNecessary((step +: steps).steps, step.inference).get
          stepsToReturn += DerivationStep(elidedStep.provenStatement.get, step.inference, elidedStep)
        case None =>
          stepsToReturn ++= removeNonEndStructuralSimplifications(steps)
      }
    }

    for (currentStep <- steps) {
      provingContext.entryContext.typeStatementDefinitionsByType.find { case (_, definitions) =>
        definitions.mapCollect(_.deconstructionInference).map(_.id).contains(currentStep.inference.id)
      } match {
        case Some((typeSymbol, _)) =>
          groupSteps(removeUnnecessarySimplifications(currentUngroupedSteps.result(), typeSymbol))
          currentMainStep = Some(currentStep)
          currentUngroupedSteps.clear()
        case None =>
          currentUngroupedSteps += currentStep
      }
    }
    groupSteps(currentUngroupedSteps.result())
    stepsToReturn.result()
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
  def createDerivationForInferenceExtraction(
    assertionStep: Step.Assertion,
    derivationSteps: Seq[DerivationStep])(
    implicit stepProvingContext: StepProvingContext
  ): DerivationStep = {
    val updatedSteps = groupStepsByDefinition(derivationSteps, Some(DerivationStep.fromAssertion(assertionStep)))
    val elidedStep = Step.Elided.ifNecessary(updatedSteps.steps, assertionStep.inference).get
    DerivationStep(elidedStep.provenStatement.get, assertionStep.inference, elidedStep)
  }

  def getInferenceExtractionWithoutPremises(
    inference: Inference,
    substitutions: Substitutions,
    extractionOption: ExtractionOption)(
    implicit stepProvingContext: StepProvingContext
  ): Option[DerivationStep] = {
    for {
      assertionStep <- Step.Assertion.forInference(inference, substitutions)
      extractionApplication <- ExtractionHelper.applyExtractionsForInference(assertionStep, extractionOption.extractionInferences, inference, substitutions, None, None, _ => (Nil, Nil)).toOption
    } yield createDerivationForInferenceExtraction(assertionStep, extractionApplication.extractionSteps)
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
      ExtractionApplication(extractionResult, mainPremise, extractionSteps, extractionPremises, extractionTargets) <- ExtractionHelper.applyExtractionsForInference(mainAssertion, extractionInferences, inference, substitutions, intendedPremises, intendedConclusion, PremiseFinder.findPremiseStepsOrTargets)
      mainAssertionWithCorrectConclusion = mainAssertion.copy(statement = mainPremise)
      extractionStep = createDerivationForInferenceExtraction(mainAssertionWithCorrectConclusion, extractionSteps)
      assertionWithExtractionStep = Step.Elided.ifNecessary((mainPremises ++ extractionPremises :+ extractionStep).deduplicate.steps, inference).get
    } yield (DerivationStep(extractionResult, inference, assertionWithExtractionStep), mainTargets ++ extractionTargets)
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
      ExtractionApplication(extractionResult, _, extractionSteps, extractionPremises, extractionTargets) <- ExtractionHelper.applyExtractionsForPremise(premise, extractionInferences, substitutions, intendedPremises, intendedConclusion, PremiseFinder.findPremiseStepsOrTargets)
      extractionStep = Step.Elided.ifNecessary(extractionSteps.steps, "Extracted")
      assertionWithExtractionStep = Step.Elided.ifNecessary(extractionPremises.deduplicate.steps ++ extractionStep.toSeq, "Extracted")
    } yield (extractionResult, assertionWithExtractionStep, extractionTargets)
  }
}
