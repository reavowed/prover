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
      val matchingInferences = availableInferences.map { inference =>
        inference.conclusion.statement.calculateSubstitutions(assertion, Substitutions.empty).map(inference -> _)
      }.collect {
        case Some(t) => t
      }
      val proofsViaFacts = matchingInferences.iterator.flatMap { case (inference, substitutions) =>
        matchPremisesToFacts(inference.premises, substitutions).map(inference -> _)
      }
      val proofsViaDirectInferences = matchingInferences.iterator.flatMap { case (inference, substitutions) =>
        matchPremisesToFactsOrDirectInferences(inference.premises, substitutions).map(inference -> _)
      }
      val proofsViaIndirectInferences = matchingInferences.iterator.flatMap { case (inference, substitutions) =>
        matchPremisesToFactsOrIndirectInferences(inference.premises, substitutions).map(inference -> _)
      }

      val allProofs = proofsViaFacts ++ proofsViaDirectInferences ++ proofsViaIndirectInferences

      val stepIterator = allProofs.map { case (inference, (matchedPremises, substitutions)) =>
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
      substitutions: Substitutions
    ): Iterator[(Seq[MatchedPremise], Substitutions)] = {
      matchPremises(premises, substitutions, matchPremiseToFacts)
    }

    private def matchPremisesToFactsOrDirectInferences(
      premises: Seq[Premise],
      substitutions: Substitutions
    ): Iterator[(Seq[MatchedPremise], Substitutions)] = {
      matchPremises(premises, substitutions, matchPremiseToFactsOrDirectInferences)
    }

    private def matchPremisesToFactsOrIndirectInferences(
      premises: Seq[Premise],
      substitutions: Substitutions
    ): Iterator[(Seq[MatchedPremise], Substitutions)] = {
      matchPremises(premises, substitutions, matchPremiseToFactsOrIndirectInferences)
    }

    private def matchPremises(
      premises: Seq[Premise],
      substitutions: Substitutions,
      premiseMatcher: (Premise, Substitutions) => Iterator[(MatchedPremise, Substitutions)]
    ): Iterator[(Seq[MatchedPremise], Substitutions)] = {
      val initial = Iterator((Seq.empty[MatchedPremise], substitutions))
      premises.foldLeft(initial) { case (acc, premise) =>
        acc.flatMap { case (matchedPremisesSoFar, substitutionsSoFar) =>
          premiseMatcher(premise, substitutionsSoFar).map { case (matchedPremise, newSubstitutions) =>
            (matchedPremisesSoFar :+ matchedPremise, newSubstitutions)
          }
        }
      }
    }

    private def matchPremiseToFacts(
      inferencePremise: Premise,
      substitutionsSoFar: Substitutions
    ): Iterator[(MatchedPremise, Substitutions)] = {
      inferencePremise match {
        case directPremise: DirectPremise =>
          matchDirectPremiseToFacts(directPremise, substitutionsSoFar)
        case deducedPremise: DeducedPremise =>
          matchDeducedPremiseToFacts(deducedPremise, substitutionsSoFar)
      }
    }

    private def matchPremiseToFactsOrDirectInferences(
      inferencePremise: Premise,
      substitutionsSoFar: Substitutions
    ): Iterator[(MatchedPremise, Substitutions)] = {
      matchPremiseToFacts(inferencePremise, substitutionsSoFar) ++
        (inferencePremise match {
          case directPremise: DirectPremise =>
            Iterator.empty
          case deducedPremise: DeducedPremise =>
            matchDeducedPremiseToDirectInferences(deducedPremise, substitutionsSoFar)
        })
    }

    private def matchPremiseToFactsOrIndirectInferences(
      inferencePremise: Premise,
      substitutionsSoFar: Substitutions
    ): Iterator[(MatchedPremise, Substitutions)] = {
      matchPremiseToFactsOrDirectInferences(inferencePremise, substitutionsSoFar) ++
        (inferencePremise match {
          case directPremise: DirectPremise =>
            Iterator.empty
          case deducedPremise: DeducedPremise =>
            matchDeducedPremiseToIndirectInferences(deducedPremise, substitutionsSoFar)
        })
    }

    private def matchDirectPremiseToFacts(
      inferencePremise: DirectPremise,
      substitutionsSoFar: Substitutions
    ): Iterator[(MatchedPremise, Substitutions)] = {
      provenAssertions.iterator.map { case ReferencedAssertion(provenStatement, reference) =>
        inferencePremise.statement.calculateSubstitutions(provenStatement.statement, substitutionsSoFar)
          .map((MatchedDirectPremise(inferencePremise, provenStatement, reference), _))
      }.collectDefined
    }

    private def matchDeducedPremiseToFacts(
      inferencePremise: DeducedPremise,
      substitutionsSoFar: Substitutions
    ): Iterator[(MatchedPremise, Substitutions)] = {
      provenDeductions.iterator.map { case ReferencedDeduction(provenAssumption, provenDeduction, reference) =>
        inferencePremise.antecedent.calculateSubstitutions(provenAssumption, substitutionsSoFar)
          .flatMap(inferencePremise.consequent.calculateSubstitutions(provenDeduction.statement, _))
          .map((MatchedDeducedPremise(inferencePremise, provenAssumption, provenDeduction, reference), _))
      }.collectDefined
    }

    private def matchDeducedPremiseToDirectInferences(
      inferencePremise: DeducedPremise,
      substitutionsSoFar: Substitutions
    ): Iterator[(MatchedPremise, Substitutions)] = {
      availableInferences.iterator.flatMap { inference =>
        matchDeducedPremiseToDirectInference(inferencePremise, inference, substitutionsSoFar)
      }
    }

    private def matchDeducedPremiseToIndirectInferences(
      inferencePremise: DeducedPremise,
      substitutionsSoFar: Substitutions
    ): Iterator[(MatchedPremise, Substitutions)] = {
      availableInferences.iterator.flatMap { inference =>
        matchDeducedPremiseToIndirectInference(inferencePremise, inference, substitutionsSoFar)
      }
    }

    private def matchDeducedPremiseToDirectInference(
      originalPremise: DeducedPremise,
      inference: Inference,
      startingPremiseSubstitutions: Substitutions
    ): Iterator[(MatchedPremise, Substitutions)] = {
      inference match {
        case Inference(_, Seq(DirectPremise(inferenceAssumption)), inferenceConclusion) =>
          matchDeducedPremiseToAssumptionAndConclusion(
            originalPremise,
            inferenceAssumption,
            inferenceConclusion,
            startingPremiseSubstitutions,
            Substitutions.empty
          ) map { case (condensedAssumption, condensedConclusion, premiseSubstitutions, inferenceSubstitutions) =>
            val reference = InferenceReference(inference, Nil, inferenceSubstitutions)
            val matchedPremise = MatchedDeducedPremise(originalPremise, condensedAssumption, condensedConclusion, reference)
            (matchedPremise, premiseSubstitutions)
          } toIterator
        case _ =>
          Iterator.empty
      }
    }

    private def matchDeducedPremiseToIndirectInference(
      originalPremise: DeducedPremise,
      inference: Inference,
      startingPremiseSubstitutions: Substitutions
    ): Iterator[(MatchedPremise, Substitutions)] = {
      inference match {
        case Inference(_, initialPremises :+ DirectPremise(inferenceAssumption), inferenceConclusion) =>
          for {
            (matchedInitialPremises, inferenceSubstitutionsAfterInitialPremises) <-
            matchPremisesToFacts(initialPremises, Substitutions.empty)
            (condensedAssumption, condensedConclusion, premiseSubstitutions, inferenceSubstitutions) <-
            matchDeducedPremiseToAssumptionAndConclusion(
              originalPremise,
              inferenceAssumption,
              inferenceConclusion,
              startingPremiseSubstitutions,
              inferenceSubstitutionsAfterInitialPremises)
          } yield {
            val reference = InferenceReference(inference, matchedInitialPremises.map(_.reference), inferenceSubstitutions)
            val matchedPremise = MatchedDeducedPremise(originalPremise, condensedAssumption, condensedConclusion, reference)
            (matchedPremise, premiseSubstitutions)
          }
        case _ =>
          Iterator.empty
      }
    }

    private def matchDeducedPremiseToAssumptionAndConclusion(
      originalPremise: DeducedPremise,
      assumption: Statement,
      conclusion: ProvenStatement,
      startingPremiseSubstitutions: Substitutions,
      startingInferenceSubstitutions: Substitutions
    ): Option[(Statement, ProvenStatement, Substitutions, Substitutions)] = {
      for {
        (premiseSubstitutions, inferenceSubstitutions) <- condenseStatements(
          Seq((originalPremise.antecedent, assumption), (originalPremise.consequent, conclusion.statement)),
          startingPremiseSubstitutions,
          startingInferenceSubstitutions)
        condensedAssumption <- Try(assumption.applySubstitutions(inferenceSubstitutions)).toOption
        condensedConclusion <- Try(conclusion.applySubstitutions(inferenceSubstitutions)).toOption
      } yield {
        (condensedAssumption, condensedConclusion, premiseSubstitutions, inferenceSubstitutions)
      }
    }

    private def condenseStatements(
      pairs: Seq[(Statement, Statement)],
      initialLeftSubstitutions: Substitutions,
      initialRightSubstitutions: Substitutions
    ): Option[(Substitutions, Substitutions)] = {
      condenseStatements(pairs, Nil, initialLeftSubstitutions, initialRightSubstitutions)
    }

    private def condenseStatements(
      toMatch: Seq[(Statement, Statement)],
      unmatched: Seq[(Statement, Statement)],
      initialLeftSubstitutions: Substitutions,
      initialRightSubstitutions: Substitutions
    ): Option[(Substitutions, Substitutions)] = {
      toMatch match {
        case Nil =>
          if (unmatched.isEmpty)
            Some((initialLeftSubstitutions, initialRightSubstitutions))
          else
            None
        case (left, right) +: othersToMatch =>
          condenseEither(left, right, initialLeftSubstitutions, initialRightSubstitutions) match {
            case Some((updatedLeftSubstitutions, updatedRightSubstitutions)) =>
              condenseStatements(unmatched ++ othersToMatch, Nil, updatedLeftSubstitutions, updatedRightSubstitutions)
            case None =>
              condenseStatements(othersToMatch, unmatched :+ (left, right), initialLeftSubstitutions, initialRightSubstitutions)
          }
      }
    }

    private def condenseEither(
      left: Statement,
      right: Statement,
      leftSubstitutions: Substitutions,
      rightSubstitutions: Substitutions
    ): Option[(Substitutions, Substitutions)] = {
      condenseLeft(left, right, leftSubstitutions, rightSubstitutions).map(leftSubstitutions -> _)
        .orElse(condenseLeft(right, left, rightSubstitutions, leftSubstitutions).map(_ -> rightSubstitutions))
    }

    private def condenseLeft(
      left: Statement,
      right: Statement,
      leftSubstitutions: Substitutions,
      rightSubstitutions: Substitutions
    ): Option[Substitutions] = {
      for {
        substitutedLeft <- Try(left.applySubstitutions(leftSubstitutions)).toOption
        updatedRightSubstitutions <- right.calculateSubstitutions(substitutedLeft, rightSubstitutions)
      } yield {
        updatedRightSubstitutions
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
