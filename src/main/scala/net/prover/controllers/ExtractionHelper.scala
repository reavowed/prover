package net.prover.controllers

import net.prover.model._
import net.prover.model.expressions.{DefinedStatement, Statement}
import net.prover.model.proof.{Premise, PremiseFinder, Step, StepProvingContext, SubstitutionContext}
import net.prover.model.proof.SubstatementExtractor.VariableTracker

import scala.util.{Success, Try}

object ExtractionHelper {
  case class ExtractionApplication(extractionSteps: Seq[Step.Assertion], premiseSteps: Seq[Step.Assertion], targetSteps: Seq[Step.Target]) {
    def addExtractionStep(step: Step.Assertion): ExtractionApplication = copy(extractionSteps = extractionSteps :+ step)
    def addPremiseSteps(steps: Seq[Step.Assertion]): ExtractionApplication = copy(premiseSteps = premiseSteps ++ steps)
    def addTargetSteps(steps: Seq[Step.Target]): ExtractionApplication = copy(targetSteps = targetSteps ++ steps)
  }
  def applySpecification(
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
      boundVariableName <- currentStatement.asOptionalInstanceOf[DefinedStatement].flatMap(_.scopedBoundVariableNames.single).orBadRequest(s"Statement ${currentStatement} did not have a single variable")
      (newName, newVariableTracker) = variableTracker.getAndAddUniqueVariableName(boundVariableName)
      term <- mainSubstitutions.terms.get(newName).orBadRequest(s"Substitutions did not specify a term '$newName'")
      extractionSubstitutions = extractionSubstitutionsWithoutVariable.copy(terms = extractionSubstitutionsWithoutVariable.terms + (variableName -> term))
      extractedConclusion <- specificationInference.conclusion.applySubstitutions(extractionSubstitutions).orBadRequest(s"Could not get extraction conclusion for inference ${specificationInference.id}")
      assertionStep = Step.Assertion(extractedConclusion, specificationInference.summary, Seq(Premise.Pending(currentStatement)), extractionSubstitutions)
    } yield (assertionStep, newVariableTracker)
  }
  def applySimpleExtraction(
    currentStatement: Statement,
    inference: Inference)(
    implicit stepProvingContext: StepProvingContext
  ): Try[(Step.Assertion, Seq[Step.Assertion], Seq[Step.Target])] = {
    for {
      (extractionPremise, otherPremises) <- +:.unapply(inference.premises).filter(_._1.requiredSubstitutions.contains(inference.requiredSubstitutions)).orBadRequest(s"Inference ${inference.id} did not have an extraction premise")
      extractionSubstitutions <- extractionPremise.calculateSubstitutions(currentStatement).flatMap(_.confirmTotality).orBadRequest(s"Could not apply extraction premise for inference ${inference.id}")
      extractedConclusion <- inference.conclusion.applySubstitutions(extractionSubstitutions).orBadRequest(s"Could not get extraction conclusion for inference ${inference.id}")
      substitutedPremises <- otherPremises.map(_.applySubstitutions(extractionSubstitutions).orBadRequest(s"Could not apply substitutions to premise")).traverseTry
      (premiseSteps, targetSteps) = PremiseFinder.findPremiseStepsOrTargets(substitutedPremises)
      assertionStep = Step.Assertion(extractedConclusion, inference.summary, (currentStatement +: substitutedPremises).map(Premise.Pending), extractionSubstitutions)
    } yield (assertionStep, premiseSteps, targetSteps)
  }
  def applyExtractions(statement: Statement, extractionInferences: Seq[Inference], baseInference: Inference, substitutions: Substitutions)(implicit stepProvingContext: StepProvingContext): Try[(Statement, ExtractionApplication)] = {
    def helper(currentStatement: Statement, inferencesRemaining: Seq[Inference], applicationSoFar: ExtractionApplication, variableTracker: VariableTracker): Try[(Statement, ExtractionApplication)] = {
      inferencesRemaining match {
        case inference +: tailInferences =>
          stepProvingContext.provingContext.specificationInferenceOption.filter(_._1 == inference)
            .map { case (_, singlePremise, predicateName, variableName) =>
              applySpecification(currentStatement, inference, singlePremise, predicateName, variableName, variableTracker, substitutions)
                .flatMap { case (assertion, newVariableTracker) =>
                  helper(assertion.statement, tailInferences, applicationSoFar.addExtractionStep(assertion), newVariableTracker)
                }
            } getOrElse applySimpleExtraction(currentStatement, inference)
            .flatMap { case (assertion, premiseSteps, targetSteps) =>
              helper(assertion.statement, tailInferences, applicationSoFar.addExtractionStep(assertion).addPremiseSteps(premiseSteps).addTargetSteps(targetSteps), variableTracker)
            }
        case Nil =>
          Success((currentStatement, applicationSoFar))
      }
    }
    helper(statement, extractionInferences, ExtractionApplication(Nil, Nil, Nil), VariableTracker(baseInference.requiredSubstitutions.terms))
  }
}
