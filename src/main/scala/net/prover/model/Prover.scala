package net.prover.model

import net.prover.model.DetailedProof._
import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise}

case class Prover(
  assertion: Statement,
  nonDistinctVariables: Set[(Variable, Variable)],
  provenAssertions: Seq[ReferencedAssertion],
  provenDeductions: Seq[ReferencedDeduction],
  premises: Seq[Premise],
  assumptions: Seq[Statement],
  debug: Boolean)(
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
    proveAssertionWithNoTransforms()
      .orElse(proveAssertionWithTransforms())
      .getOrElse(throw new Exception(s"Could not prove statement $assertion"))
  }

  def proveAssertionWithNoTransforms(): Option[AssertionStep] = {
    availableInferences.iterator
      .flatMap { inference =>
        inference.conclusion.statement.calculateSubstitutions(assertion, PartialSubstitutions.empty).map(inference -> _)
      }
      .flatMap { case (inference, substitutions) =>
        matchPremisesToFacts(inference.premises, substitutions).map(inference -> _)
      }
      .flatMap { case (inference, (matchedPremises, substitutions)) =>
        substitutions.tryResolve().map((inference, matchedPremises, _))
      }
      .mapCollect { case (inference, matchedPremises, substitutions) =>
        makeAssertionStep(assertion, inference, matchedPremises, substitutions)
      }
      .nextOption()
  }

  def proveAssertionWithTransforms(): Option[AssertionStep] = {
    context.inferenceTransforms.iterator
      .flatMap { transform =>
        availableInferences.iterator.map(transform -> _)
      }
      .mapCollect { case (transform, inference) =>
        inference.premises.toType[DirectPremise].map((transform, inference, _))
      }
      .flatMap { case (transform, inference, inferencePremises) =>
        val conclusion = inference.conclusion.statement
        transform.transform(inferencePremises, conclusion).iterator
          .map { case (transformedPremises, statementsToProve) =>
            (inference, inferencePremises, transformedPremises, statementsToProve)
          }
      }
      .flatMap { case (inference, inferencePremises, transformedPremises, statementsToProve) =>
        val transformedConclusion = statementsToProve.last
        val oldVariables = (inferencePremises.map(_.statement) :+ inference.conclusion.statement).map(_.allVariables).foldTogether
        val newVariables = (transformedPremises.map(_.statement) :+ transformedConclusion).map(_.allVariables).foldTogether diff oldVariables
        val newNonDistinctVariables = for {
          newVariable <- newVariables.all
          oldVariable <- oldVariables.all
        } yield newVariable -> oldVariable
        for {
          substitutions <- transformedConclusion.calculateSubstitutions(assertion, PartialSubstitutions.empty)
          proofSteps <- statementsToProve.collectFold[AssertionStep] { case (assertions, statement) =>
            val provenAssertions = transformedPremises.mapWithIndex { (premise, index) =>
              ReferencedAssertion(ProvenStatement.withNoConditions(premise.statement), DirectReference(index))
            } ++ assertions.mapWithIndex { (step, index) =>
              ReferencedAssertion(step.provenStatement, DirectReference(transformedPremises.length + index))
            }
            Prover(
              statement,
              newNonDistinctVariables,
              provenAssertions,
              Nil,
              transformedPremises,
              Nil,
              false
            ).proveAssertionWithNoTransforms()
          }.toSeq
        } yield {
          val transformedInference = DerivedInference(
            inference.name,
            transformedPremises,
            proofSteps.last.provenStatement)
          (substitutions, transformedInference)
        }
      }
      .flatMap { case (substitutions, transformedInference) =>
        matchPremisesToFacts(transformedInference.premises, substitutions).map((transformedInference, _))
      }
      .flatMap { case (transformedInference, (matchedPremises, substitutions)) =>
        substitutions.tryResolve().map((transformedInference, matchedPremises, _))
      }
      .mapCollect { case (transformedInference, matchedPremises, substitutions) =>
        makeAssertionStep(assertion, transformedInference, matchedPremises, substitutions)
      }
      .nextOption()
  }

  private def matchPremisesToFacts(
    premises: Seq[Premise],
    substitutions: PartialSubstitutions
  ): Iterator[(Seq[PremiseMatch], PartialSubstitutions)] = {
    val initial = Iterator((Seq.empty[PremiseMatch], substitutions))
    premises.foldLeft(initial) { case (acc, premise) =>
      acc.flatMap { case (matchedPremisesSoFar, substitutionsSoFar) =>
        matchPremiseToFacts(premise, substitutionsSoFar).map { case (matchedPremise, newSubstitutions) =>
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
    provenAssertions.iterator.flatMap { case ReferencedAssertion(provenStatement, reference) =>
      inferencePremise.statement.calculateSubstitutions(provenStatement.statement, substitutionsSoFar)
        .map { newSubstitutions =>
          (DirectPremiseMatch(provenStatement, reference), newSubstitutions)
        }
    }
  }

  private def matchDeducedPremiseToFacts(
    inferencePremise: DeducedPremise,
    substitutionsSoFar: PartialSubstitutions
  ): Iterator[(PremiseMatch, PartialSubstitutions)] = {
    provenDeductions.iterator.flatMap { case ReferencedDeduction(provenAssumption, provenDeduction, reference) =>
      inferencePremise.antecedent.calculateSubstitutions(provenAssumption, substitutionsSoFar)
        .flatMap(inferencePremise.consequent.calculateSubstitutions(provenDeduction.statement, _))
        .map((DeducedPremiseMatch(provenAssumption, provenDeduction, reference), _))
    }
  }

  private def makeAssertionStep(
    assertion: Statement,
    inference: Inference,
    matchedPremises: Seq[PremiseMatch],
    substitutions: Substitutions
  ): Option[AssertionStep] = {
    for {
      substitutedInference <- inference.applySubstitutions(substitutions)
      combinedConditions <- (matchedPremises.map(_.provenStatement.conditions) :+ substitutedInference.conclusion.conditions)
        .reduce(_ ++ _)
        .addDistinctVariables(substitutions.distinctVariables)
        .restrictToStatements(premises.flatMap(_.statements) ++ assumptions :+ substitutedInference.conclusion.statement)
        .addDistinctVariables(substitutedInference.conclusion.conditions.arbitraryVariables, assumptions)
      if !nonDistinctVariables.exists { case (first, second) =>
        combinedConditions.distinctVariables.areDistinct(first, second)
      }
      provenStatement = ProvenStatement(assertion, combinedConditions)
    } yield {
      AssertionStep(provenStatement, inference, matchedPremises.map(_.reference), substitutions)
    }
  }
}
