package net.prover.model

import net.prover.model.DetailedProof._
import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise}

import scala.util.Try

case class Prover(
  assertion: Statement,
  targetConditions: Option[Conditions],
  provenAssertions: Seq[ReferencedAssertion],
  provenDeductions: Seq[ReferencedDeduction],
  premises: Seq[Premise],
  assumptions: Seq[Statement])(
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
      .mapCollect { inference =>
        inference.conclusion.statement.calculateSubstitutions(assertion, PartialSubstitutions.empty).map(inference -> _)
      }
      .flatMap { case (inference, substitutions) =>
        matchPremisesToFacts(inference.premises, substitutions).map(inference -> _)
      }
      .mapCollect { case (inference, (matchedPremises, substitutions)) =>
        substitutions.tryResolve().map((inference, matchedPremises, _))
      }
      .mapCollect { case (inference, matchedPremises, (substitutions, distinctVariables)) =>
        makeAssertionStep(assertion, inference, matchedPremises, substitutions, distinctVariables)
      }
      .nextOption()
  }

  def proveAssertionWithTransforms(): Option[AssertionStep] = {
    context.inferenceTransforms.iterator
      .flatMap { transform =>
        availableInferences.iterator.map(transform -> _)
      }
      .mapCollect { case (transform, inference) =>
        for {
          inferencePremises <- inference.premises.toType[DirectPremise]
          conclusion = inference.conclusion.statement
          transformedConclusion = transform.transformStatement(conclusion)
          substitutions <- transformedConclusion.calculateSubstitutions(assertion, PartialSubstitutions.empty)
          transformedPremises = inferencePremises.map(transform.transformPremise)
          statementsToProve = inferencePremises.map(_.statement) :+ conclusion :+ transformedConclusion
          proofSteps <- statementsToProve.collectFold[AssertionStep] { case (assertions, statement) =>
            val provenAssertions = transformedPremises.mapWithIndex { (premise, index) =>
              ReferencedAssertion(ProvenStatement.withNoConditions(premise.statement), DirectReference(index))
            } ++ assertions.mapWithIndex { (step, index) =>
              ReferencedAssertion(step.provenStatement, DirectReference(transformedPremises.length + index))
            }
            Prover(
              statement,
              Some(Conditions.empty),
              provenAssertions,
              Nil,
              transformedPremises,
              Nil
            ).proveAssertionWithNoTransforms()
          }
        } yield {
          val transformedInference = new Inference {
            val name = inference.name
            val premises = transformedPremises
            val conclusion = proofSteps.last.provenStatement
          }
          (substitutions, transformedInference)
        }
      }
      .flatMap { case (substitutions, transformedInference) =>
        matchPremisesToFacts(transformedInference.premises, substitutions).map((transformedInference, _))
      }
      .mapCollect { case (transformedInference, (matchedPremises, substitutions)) =>
        substitutions.tryResolve().map((transformedInference, matchedPremises, _))
      }
      .mapCollect { case (transformedInference, matchedPremises, (substitutions, distinctVariables)) =>
        makeAssertionStep(assertion, transformedInference, matchedPremises, substitutions, distinctVariables)
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
    provenAssertions.iterator.mapCollect { case ReferencedAssertion(provenStatement, reference) =>
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
    provenDeductions.iterator.mapCollect { case ReferencedDeduction(provenAssumption, provenDeduction, reference) =>
      inferencePremise.antecedent.calculateSubstitutions(provenAssumption, substitutionsSoFar)
        .flatMap(inferencePremise.consequent.calculateSubstitutions(provenDeduction.statement, _))
        .map((DeducedPremiseMatch(provenAssumption, provenDeduction, reference), _))
    }
  }

  private def makeAssertionStep(
    assertion: Statement,
    inference: Inference,
    matchedPremises: Seq[PremiseMatch],
    substitutions: Substitutions,
    distinctVariables: Map[TermVariable, Variables]
  ): Option[AssertionStep] = {
    for {
      substitutedInference <- inference.applySubstitutions(substitutions)
      boundVariables =
        (premises.map(_.boundVariables) ++
          assumptions.map(_.boundVariables) :+
          substitutedInference.conclusion.statement.boundVariables
        ).reduce(_ intersect _)
      activeVariables =
        (premises.map(_.allVariables) ++
          assumptions.map(_.allVariables) :+
          substitutedInference.conclusion.statement.allVariables
        ).reduce(_ ++ _)
      unrestrictedConditions = (matchedPremises.map(_.provenStatement.conditions) :+ substitutedInference.conclusion.conditions)
        .reduce(_ ++ _)
      restrictedConditions = unrestrictedConditions
        .filterOutBoundVariables(boundVariables)
        .restrictToActiveVariables(activeVariables)
      expandedConditions <- restrictedConditions
        .addDistinctVariables(distinctVariables)
        .addDistinctVariables(restrictedConditions.arbitraryVariables, assumptions)
      if !targetConditions.exists(_ != expandedConditions)
      provenStatement = ProvenStatement(assertion, expandedConditions)
    } yield {
      AssertionStep(provenStatement, inference, matchedPremises.map(_.reference), substitutions)
    }
  }
}
