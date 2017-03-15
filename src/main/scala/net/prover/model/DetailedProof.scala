package net.prover.model

import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise}

import scala.util.Try

case class DetailedProof(steps: Seq[DetailedProof.Step])

object DetailedProof {
  sealed trait Step
  case class AssumptionStep(
      assumption: Statement,
      steps: Seq[Step])
    extends Step
  case class AssertionStep(
      provenStatement: ProvenStatement,
      inference: Inference,
      references: Seq[Reference],
      substitutions: Substitutions)
    extends Step

  sealed trait Reference
  case class DirectReference(index: Int) extends Reference
  case class DeducedReference(antecedentIndex: Int, consequentIndex: Int) extends Reference
  case class InferenceReference(inference: Inference, substitutions: Substitutions) extends Reference

  case class ReferencedAssertion(provenStatement: ProvenStatement, reference: DirectReference)
  case class ReferencedDeduction(assumption: Statement, deduction: ProvenStatement, reference: Reference)

  def fillInOutline(
    premises: Seq[Premise],
    proofOutline: ProofOutline)(
    implicit context: Context
  ): DetailedProof = {
    val premiseAssertions = premises.zipWithIndex.collect {
      case (DirectPremise(premise), index) =>
        ReferencedAssertion(ProvenStatement.withNoConditions(premise), DirectReference(index))
    }
    val premiseDeductions = premises.zipWithIndex.collect {
      case (DeducedPremise(assumption, conclusion), index) =>
        ReferencedDeduction(assumption, ProvenStatement.withNoConditions(conclusion), DirectReference(index))
    }
    val detailedSteps = proveSteps(proofOutline.steps, Nil, premiseAssertions, premiseDeductions, premises.length)
    DetailedProof(detailedSteps)
  }

  private def proveSteps(
    stepOutlines: Seq[ProofOutline.Step],
    accumulatedSteps: Seq[Step],
    provenAssertions: Seq[ReferencedAssertion],
    provenDeductions: Seq[ReferencedDeduction],
    nextReference: Int)(
    implicit context: Context
  ): Seq[Step] = {
    stepOutlines match {
      case Nil =>
        accumulatedSteps
      case stepOutline +: otherStepOutlines =>
        val step = proveStep(stepOutline, provenAssertions, provenDeductions, nextReference + 1)
        val (updatedAssertions, updatedDeductions) = step match {
          case AssumptionStep(assumption, substeps) =>
            val newDeductions = substeps.zipWithIndex.collect {
              case (AssertionStep(deduction, _, _, _), index) =>
                ReferencedDeduction(assumption, deduction, DeducedReference(nextReference, index))
            }
            (provenAssertions, provenDeductions ++ newDeductions)
          case AssertionStep(provenStatement, _, _, _) =>
            val newAssertion = ReferencedAssertion(provenStatement, DirectReference(nextReference))
            (provenAssertions :+ newAssertion, provenDeductions)
        }
        proveSteps(otherStepOutlines, accumulatedSteps :+ step, updatedAssertions, updatedDeductions, nextReference + 1)
    }
  }

  private def proveStep(
    stepOutline: ProofOutline.Step,
    provenAssertions: Seq[ReferencedAssertion],
    provenDeductions: Seq[ReferencedDeduction],
    nextReference: Int)(
    implicit context: Context
  ): Step = {
    stepOutline match {
      case ProofOutline.AssumptionStep(assumption, substepOutlines) =>
        val substeps = proveSteps(
          substepOutlines,
          Nil,
          provenAssertions :+ ReferencedAssertion(ProvenStatement.withNoConditions(assumption), DirectReference(nextReference)),
          provenDeductions,
          nextReference + 1)
        AssumptionStep(assumption, substeps)
      case ProofOutline.AssertionStep(assertion) =>
        Prover(assertion, provenAssertions, provenDeductions).proveAssertion()
    }
  }

  private case class Prover(
    assertion: Statement,
    provenAssertions: Seq[ReferencedAssertion],
    provenDeductions: Seq[ReferencedDeduction])(
    implicit context: Context)
  {
    def availableInferences: Seq[Inference] = context.inferences

    sealed trait MatchedPremise {
      def provenStatement: ProvenStatement
      def reference: Reference
    }
    case class MatchedDirectPremise(
      premise: DirectPremise,
      provenStatement: ProvenStatement,
      reference: DirectReference)
      extends MatchedPremise
    case class MatchedDeducedPremise(
      premise: DeducedPremise,
      assumption: Statement,
      provenStatement: ProvenStatement,
      reference: Reference)
      extends MatchedPremise


    def proveAssertion(): AssertionStep = {
      availableInferences.mapCollect { inference =>
        inference.conclusion.statement.calculateSubstitutions(assertion, Substitutions.empty).map(inference -> _)
      }.mapCollect { case (inference, substitutions) =>
        matchInferencePremises(inference, substitutions).map(inference -> _)
      }.map { case (inference, (matchedPremises, substitutions)) =>
        makeAssertionStep(assertion, inference, matchedPremises, substitutions)
      }.headOption.getOrElse(throw new Exception(s"Could not prove statement $assertion"))
    }

    private def matchInferencePremises(
      inference: Inference,
      substitutions: Substitutions
    ): Option[(Seq[MatchedPremise], Substitutions)] = {
      val initial = Seq((Seq.empty[MatchedPremise], substitutions))
      inference.premises.foldLeft(initial) { case (acc, premise) =>
        acc.flatMap { case (matchedPremisesSoFar, substitutionsSoFar) =>
          matchPremise(premise, substitutionsSoFar).map { case (matchedPremise, newSubstitutions) =>
            (matchedPremisesSoFar :+ matchedPremise, newSubstitutions)
          }
        }
      }.headOption
    }

    private def matchPremise(
      inferencePremise: Premise,
      substitutionsSoFar: Substitutions
    ): Seq[(MatchedPremise, Substitutions)] = {
      inferencePremise match {
        case directPremise: DirectPremise =>
          matchDirectPremise(directPremise, substitutionsSoFar)
        case deducedPremise: DeducedPremise =>
          matchDeducedPremise(deducedPremise, substitutionsSoFar)
      }
    }

    private def matchDirectPremise(
      inferencePremise: DirectPremise,
      substitutionsSoFar: Substitutions
    ): Seq[(MatchedPremise, Substitutions)] = {
      provenAssertions.map { case ReferencedAssertion(provenStatement, reference) =>
        inferencePremise.statement.calculateSubstitutions(provenStatement.statement, substitutionsSoFar)
          .map((MatchedDirectPremise(inferencePremise, provenStatement, reference), _))
      } collectDefined
    }

    private def matchDeducedPremise(
      inferencePremise: DeducedPremise,
      substitutionsSoFar: Substitutions
    ): Seq[(MatchedPremise, Substitutions)] = {
      val provenMatches = provenDeductions.map { case ReferencedDeduction(provenAssumption, provenDeduction, reference) =>
        inferencePremise.antecedent.calculateSubstitutions(provenAssumption, substitutionsSoFar)
          .flatMap(inferencePremise.consequent.calculateSubstitutions(provenDeduction.statement, _))
          .map((MatchedDeducedPremise(inferencePremise, provenAssumption, provenDeduction, reference), _))
      }.collectDefined
      val inferenceMatches = availableInferences.map { inference =>
        matchDeducedPremiseToInference(inferencePremise, inference, substitutionsSoFar)
      }.collectDefined
      provenMatches ++ inferenceMatches
    }

    private def matchDeducedPremiseToInference(
      originalPremise: DeducedPremise,
      inference: Inference,
      substitutionsSoFar: Substitutions
    ): Option[(MatchedPremise, Substitutions)] = {
      inference match {
        case Inference(_, Seq(DirectPremise(inferenceAssumption)), inferenceConclusion) =>
          for {
            condensedAssumption <- Try(originalPremise.antecedent.applySubstitutions(substitutionsSoFar)).toOption
            inferenceSubstitutions <- inferenceAssumption.calculateSubstitutions(condensedAssumption, Substitutions.empty)
            condensedConclusion <- Try(inferenceConclusion.applySubstitutions(inferenceSubstitutions)).toOption
            premiseSubstitutions <- originalPremise.consequent.calculateSubstitutions(
              condensedConclusion.statement,
              substitutionsSoFar)
          } yield {
            val reference = InferenceReference(inference, inferenceSubstitutions)
            val matchedPremise = MatchedDeducedPremise(originalPremise, condensedAssumption, condensedConclusion, reference)
            (matchedPremise, premiseSubstitutions)
          }
        case _ =>
          None
      }
    }

    private def makeAssertionStep(
      assertion: Statement,
      inference: Inference,
      matchedPremises: Seq[MatchedPremise],
      substitutions: Substitutions
    ): AssertionStep = {
      val substitutedInference = inference.applySubstitutions(substitutions)
      val arbitraryVariables = matchedPremises.map(_.provenStatement.arbitraryVariables)
        .foldLeft(substitutedInference.conclusion.arbitraryVariables)(_ ++ _)
      val distinctVariables = matchedPremises.map(_.provenStatement.distinctVariables)
        .foldLeft(substitutedInference.conclusion.distinctVariables)(_ ++ _)
      val provenStatement = ProvenStatement(assertion, arbitraryVariables, distinctVariables)
      AssertionStep(provenStatement, inference, matchedPremises.map(_.reference), substitutions)
    }
  }
}
