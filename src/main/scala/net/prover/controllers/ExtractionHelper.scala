package net.prover.controllers

import net.prover.exceptions.BadRequestException
import net.prover.model._
import net.prover.model.expressions.{DefinedStatement, Statement, TermVariable}
import net.prover.model.proof.SubstatementExtractor.VariableTracker
import net.prover.model.proof.{Premise, Step, StepContext, StepProvingContext, SubstitutionContext}

import scala.util.{Failure, Success, Try}

object ExtractionHelper {
  case class ExtractionApplication(result: Statement, mainPremise: Statement, extractionSteps: Seq[Step.Assertion], premiseSteps: Seq[Step], targetSteps: Seq[Step.Target])

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
    findPremiseStepsOrTargets: Seq[Statement] => (Seq[Step], Seq[Step.Target]))(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
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
    } yield ExtractionApplication(innerResult, updatedMainPremiseWithVariable, assertionStep +: innerSteps, innerPremises, innerTargets)
  }
  private def applySimpleExtraction(
    currentStatement: Statement,
    inference: Inference,
    variableTracker: VariableTracker,
    inferencesRemaining: Seq[Inference],
    mainSubstitutions: Substitutions,
    intendedPremisesOption: Option[Seq[Statement]],
    intendedConclusion: Option[Statement],
    findPremiseStepsOrTargets: Seq[Statement] => (Seq[Step], Seq[Step.Target]))(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
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
        case Some(premises) =>
          for {
            substitutedPremises <- premises.map(p => p.applySubstitutions(mainSubstitutions).orBadRequest(s"Could not apply substitutions to intended premise $p")).traverseTry
            substitutions <- otherPremises.zip(substitutedPremises).foldLeft(Try(Substitutions.Possible.empty)) { case (substitutionsSoFarTry, (otherPremise, intendedPremise)) =>
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
    } yield ExtractionApplication(innerResult, updatedMainPremise, assertionStep +: innerSteps, premiseSteps ++ innerPremises, targetSteps ++ innerTargets)
  }

  private def applyExtractions(
    currentStatement: Statement,
    inferencesRemaining: Seq[Inference],
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement],
    variableTracker: VariableTracker,
    findPremiseStepsOrTargets: Seq[Statement] => (Seq[Step], Seq[Step.Target]))(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Try[ExtractionApplication] = {
    inferencesRemaining match {
      case inference +: tailInferences =>
        provingContext.specificationInferenceOption.filter(_._1 == inference)
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

  private def isStructuralSimplification(inference: Inference)(implicit provingContext: ProvingContext): Boolean = provingContext.structuralSimplificationInferences.exists(_._1 == inference)
  private def removeNonEndStructuralSimplifications(
    extractionApplication: ExtractionApplication)(
    implicit provingContext: ProvingContext
  ): ExtractionApplication = {
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

  def removeAllStructuralSimplifications(
    extractionApplication: ExtractionApplication)(
    implicit provingContext: ProvingContext
  ): ExtractionApplication = {
    extractionApplication.copy(extractionSteps = extractionApplication.extractionSteps.filter(s => !isStructuralSimplification(s.inference)))
  }

  def applyExtractions(
    premise: Statement,
    extractionInferences: Seq[Inference],
    baseInference: Inference,
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement],
    findPremiseStepsOrTargets: Seq[Statement] => (Seq[Step], Seq[Step.Target]))(
    implicit provingContext: ProvingContext,
    substitutionContext: SubstitutionContext
  ): Try[ExtractionApplication] = {
    applyExtractions(premise, extractionInferences, substitutions, intendedPremises, intendedConclusion, VariableTracker.fromInference(baseInference), findPremiseStepsOrTargets).map(removeNonEndStructuralSimplifications)
  }
  def applyExtractions(
    premise: Premise,
    extractionInferences: Seq[Inference],
    substitutions: Substitutions,
    intendedPremises: Option[Seq[Statement]],
    intendedConclusion: Option[Statement],
    findPremiseStepsOrTargets: Seq[Statement] => (Seq[Step], Seq[Step.Target]))(
    implicit provingContext: ProvingContext,
    stepContext: StepContext
  ): Try[ExtractionApplication] = {
    applyExtractions(premise.statement, extractionInferences, substitutions, intendedPremises, intendedConclusion, VariableTracker.fromStepContext, findPremiseStepsOrTargets).map(removeNonEndStructuralSimplifications)
  }
}
