package net.prover.model

import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise}

case class
DetailedProof(steps: Seq[DetailedProof.Step])

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
  case class InferenceReference(inference: Inference, premiseReferences: Seq[Reference], substitutions: Substitutions) extends Reference

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
        val step = proveStep(stepOutline, provenAssertions, provenDeductions, nextReference)
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

  case class Prover(
    assertion: Statement,
    provenAssertions: Seq[ReferencedAssertion],
    provenDeductions: Seq[ReferencedDeduction])(
    implicit context: Context)
  {
    def availableInferences: Seq[Inference] = context.inferences

    sealed trait PremiseMatch {
      def provenStatement: ProvenStatement
      def reference: Reference
    }
    case class DirectPremiseMatch(
      provenStatement: ProvenStatement,
      reference: Reference)
      extends PremiseMatch
    case class DeducedPremiseMatch(
      assumption: Statement,
      provenStatement: ProvenStatement,
      reference: Reference)
      extends PremiseMatch

    def proveAssertion(): AssertionStep = {
      val stepIterator = availableInferences.iterator
        .map { inference =>
          inference.conclusion.statement.calculateSubstitutions(assertion, PartialSubstitutions.empty).map(inference -> _)
        }
        .collectDefined
        .flatMap { case (inference, substitutions) =>
          matchPremisesToFacts(inference.premises, substitutions).map(inference -> _)
        }
        .map { case (inference, (matchedPremises, substitutions)) =>
          substitutions.tryResolve().map((inference, matchedPremises, _))
        }
        .collectDefined
        .map { case (inference, matchedPremises, substitutions) =>
          makeAssertionStep(assertion, inference, matchedPremises, substitutions)
        }
      if (stepIterator.hasNext) {
        stepIterator.next()
      } else {
        throw new Exception(s"Could not prove statement $assertion")
      }
    }

    private def matchPremisesToFacts(
      premises: Seq[Premise],
      substitutions: PartialSubstitutions
    ): Iterator[(Seq[PremiseMatch], PartialSubstitutions)] = {
      val initial = Iterator((Seq.empty[PremiseMatch], substitutions))
      premises.foldLeft(initial) { case (acc, premise) =>
        acc.flatMap { case (matchedPremisesSoFar, substitutionsSoFar) =>
          matchPremiseToFacts(premise, substitutionsSoFar).toList.map { case (matchedPremise, newSubstitutions) =>
            (matchedPremisesSoFar :+ matchedPremise, newSubstitutions)
          }
        }
      }
    }

    private def matchPremiseToFacts(
      inferencePremise: Premise,
      substitutionsSoFar: PartialSubstitutions
    ): Iterator[(PremiseMatch, PartialSubstitutions)] = {
      inferencePremise match {
        case directPremise: DirectPremise =>
          matchDirectPremiseToFacts(directPremise, substitutionsSoFar)
        case deducedPremise: DeducedPremise =>
          matchDeducedPremiseToFacts(deducedPremise, substitutionsSoFar)
      }
    }

    private def matchDirectPremiseToFacts(
      inferencePremise: DirectPremise,
      substitutionsSoFar: PartialSubstitutions
    ): Iterator[(PremiseMatch, PartialSubstitutions)] = {
      provenAssertions.iterator.map { case ReferencedAssertion(provenStatement, reference) =>
        inferencePremise.statement.calculateSubstitutions(provenStatement.statement, substitutionsSoFar)
          .map((DirectPremiseMatch(provenStatement, reference), _))
      }.collectDefined
    }

    private def matchDeducedPremiseToFacts(
      inferencePremise: DeducedPremise,
      substitutionsSoFar: PartialSubstitutions
    ): Iterator[(PremiseMatch, PartialSubstitutions)] = {
      provenDeductions.iterator.map { case ReferencedDeduction(provenAssumption, provenDeduction, reference) =>
        inferencePremise.antecedent.calculateSubstitutions(provenAssumption, substitutionsSoFar)
          .flatMap(inferencePremise.consequent.calculateSubstitutions(provenDeduction.statement, _))
          .map((DeducedPremiseMatch(provenAssumption, provenDeduction, reference), _))
      }.collectDefined
    }

    private def makeAssertionStep(
      assertion: Statement,
      inference: Inference,
      matchedPremises: Seq[PremiseMatch],
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
