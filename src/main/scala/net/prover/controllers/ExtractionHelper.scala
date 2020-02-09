package net.prover.controllers

import net.prover.model._
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model.proof.{Premise, PremiseFinder, Step, StepProvingContext, SubstitutionContext}
import net.prover.model.proof.SubstatementExtractor.VariableTracker

import scala.util.{Success, Try}

object ExtractionHelper {
  case class ExtractionApplication(extractionSteps: Seq[Step.Assertion], premiseSteps: Seq[Step], targetSteps: Seq[Step.Target]) {
    def addExtractionStep(step: Step.Assertion): ExtractionApplication = copy(extractionSteps = extractionSteps :+ step)
    def addPremiseSteps(steps: Seq[Step]): ExtractionApplication = copy(premiseSteps = premiseSteps ++ steps)
    def addTargetSteps(steps: Seq[Step.Target]): ExtractionApplication = copy(targetSteps = targetSteps ++ steps)
  }
  private def applySpecification(
    currentStatement: Statement,
    specificationInference: Inference,
    extractionPremise: Statement,
    predicateName: String,
    variableName: String,
    variableTracker: VariableTracker,
    mainSubstitutions: Substitutions)(
    implicit substitutionContext: SubstitutionContext
  ): Try[(Step.Assertion, VariableTracker)] = {
    for {
      extractionSubstitutionsWithoutVariable <- extractionPremise.calculateSubstitutions(currentStatement).flatMap(_.confirmTotality).orBadRequest(s"Could not apply extraction premise for inference ${specificationInference.id}")
      boundVariableName <- currentStatement.asOptionalInstanceOf[DefinedStatement].flatMap(_.scopedBoundVariableNames.single).orBadRequest(s"Statement $currentStatement did not have a single variable")
      (newName, newVariableTracker) = variableTracker.getAndAddUniqueVariableName(boundVariableName)
      term <- mainSubstitutions.terms.get(newName).orBadRequest(s"Substitutions did not specify a term '$newName'")
      extractionSubstitutions = extractionSubstitutionsWithoutVariable.copy(terms = extractionSubstitutionsWithoutVariable.terms + (variableName -> term))
      extractedConclusion <- specificationInference.conclusion.applySubstitutions(extractionSubstitutions).orBadRequest(s"Could not get extraction conclusion for inference ${specificationInference.id}")
      assertionStep = Step.Assertion(extractedConclusion, specificationInference.summary, Seq(Premise.Pending(currentStatement)), extractionSubstitutions)
    } yield (assertionStep, newVariableTracker)
  }
  private def applySimpleExtraction(
    currentStatement: Statement,
    inference: Inference,
    findPremiseStepsOrTargets: Seq[Statement] => (Seq[Step], Seq[Step.Target]))(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Statement, Step.Assertion, Seq[Step], Seq[Step.Target])] = {
    for {
      (extractionPremise, otherPremises) <- +:.unapply(inference.premises).filter(_._1.requiredSubstitutions.contains(inference.requiredSubstitutions)).orBadRequest(s"Inference ${inference.id} did not have an extraction premise")
      extractionSubstitutions <- extractionPremise.calculateSubstitutions(currentStatement).flatMap(_.confirmTotality).orBadRequest(s"Could not apply extraction premise for inference ${inference.id}")
      extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions).orBadRequest(s"Could not get extraction conclusion for inference ${inference.id}")
      substitutedPremises <- otherPremises.map(_.applySubstitutions(extractionSubstitutions).orBadRequest(s"Could not apply substitutions to premise")).traverseTry
      (premiseSteps, targetSteps) = findPremiseStepsOrTargets(substitutedPremises)
      assertionStep = Step.Assertion(extractedConclusion, inference.summary, (currentStatement +: substitutedPremises).map(Premise.Pending), extractionSubstitutions)
    } yield (extractedConclusion, assertionStep, premiseSteps, targetSteps)
  }
  private def applyExtractions(
    currentStatement: Statement,
    inferencesRemaining: Seq[Inference],
    applicationSoFar: ExtractionApplication,
    substitutions: Substitutions,
    variableTracker: VariableTracker,
    findPremiseStepsOrTargets: Seq[Statement] => (Seq[Step], Seq[Step.Target]))(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Statement, ExtractionApplication)] = {
    inferencesRemaining match {
      case inference +: tailInferences =>
        stepProvingContext.provingContext.specificationInferenceOption.filter(_._1 == inference)
          .map { case (_, singlePremise, predicateName, variableName) =>
            applySpecification(currentStatement, inference, singlePremise, predicateName, variableName, variableTracker, substitutions)
              .flatMap { case (assertion, newVariableTracker) =>
                applyExtractions(assertion.statement, tailInferences, applicationSoFar.addExtractionStep(assertion), substitutions, newVariableTracker, findPremiseStepsOrTargets)
              }
          } getOrElse applySimpleExtraction(currentStatement, inference, findPremiseStepsOrTargets)
          .flatMap { case (result, assertion, premiseSteps, targetSteps) =>
            applyExtractions(result, tailInferences, applicationSoFar.addExtractionStep(assertion).addPremiseSteps(premiseSteps).addTargetSteps(targetSteps), substitutions, variableTracker, findPremiseStepsOrTargets)(
              stepProvingContext = stepProvingContext.copy(stepContext = stepProvingContext.stepContext.addSteps(premiseSteps))
            )
          }
      case Nil =>
        Success((currentStatement, applicationSoFar))
    }
  }
  private def removeStructuralSimplifications(
    extractionApplication: ExtractionApplication)(
    implicit provingContext: ProvingContext
  ): ExtractionApplication = {
    def isStructuralSimplification(inference: Inference): Boolean = provingContext.structuralSimplificationInferences.exists(_._1 == inference)
    @scala.annotation.tailrec
    def helper(remainingAssertions: Seq[Step.Assertion], filteredAssertions: Seq[Step.Assertion]): Seq[Step.Assertion] = {
      remainingAssertions match {
        case head +: tail if isStructuralSimplification(head.inference) && tail.exists(a => !isStructuralSimplification(a.inference)) =>
          helper(tail, filteredAssertions)
        case head +: tail =>
          helper(tail, filteredAssertions :+ head)
        case Nil =>
          filteredAssertions
      }
    }
    extractionApplication.copy(extractionSteps = helper(extractionApplication.extractionSteps, Nil))
  }
  def applyExtractions(
    statement: Statement,
    extractionInferences: Seq[Inference],
    baseInference: Inference,
    substitutions: Substitutions,
    findPremiseStepsOrTargets: Seq[Statement] => (Seq[Step], Seq[Step.Target]))(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Statement, ExtractionApplication)] = {
    applyExtractions(statement, extractionInferences, ExtractionApplication(Nil, Nil, Nil), substitutions, VariableTracker.fromInference(baseInference), findPremiseStepsOrTargets).map(_.mapRight(removeStructuralSimplifications))
  }
  def applyExtractions(
    premise: Premise,
    extractionInferences: Seq[Inference],
    substitutions: Substitutions,
    findPremiseStepsOrTargets: Seq[Statement] => (Seq[Step], Seq[Step.Target]))(
    implicit stepProvingContext: StepProvingContext): Try[(Statement, ExtractionApplication)] = {
    applyExtractions(premise.statement, extractionInferences, ExtractionApplication(Nil, Nil, Nil), substitutions, VariableTracker.fromStepContext, findPremiseStepsOrTargets).map(_.mapRight(removeStructuralSimplifications))
  }
}
