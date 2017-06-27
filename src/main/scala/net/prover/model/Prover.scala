package net.prover.model

import net.prover.model.DetailedProof._
import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise, RearrangementType}

case class Prover(
  assertion: Statement,
  nonArbitraryVariables: Set[TermVariable],
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

  def proveAssertion(): StepWithProvenStatement = {
    proveAssertionDirectlyFromInferences()
      .orElse(proveAssertionByRearranging())
      .orElse(proveAssertionFromTransformedInferences())
      .getOrElse(throw new Exception(s"Could not prove statement $assertion"))
  }

  def proveAssertionDirectlyFromInferences(): Option[AssertionStep] = {
    availableInferences.iterator
      .flatMap { inference =>
        inference.conclusion.statement.calculateSubstitutions(assertion, PartialSubstitutions.empty).map(inference -> _)
      }
      .flatMap { case (inference, substitutions) =>
        matchPremisesToFacts(inference.premises, substitutions, inference.allowsRearrangement).map(inference -> _)
      }
      .flatMap { case (inference, (matchedPremises, substitutions)) =>
        substitutions.tryResolve().map((inference, matchedPremises, _))
      }
      .mapCollect { case (inference, matchedPremises, substitutions) =>
        proveStatement(assertion, inference.conclusion, matchedPremises, substitutions)
          .map(s => AssertionStep(s, inference.summary, matchedPremises.map(_.reference)))
      }
      .nextOption()
  }

  def proveAssertionByRearranging(): Option[AssertionStep] = {
    val expansions = availableInferences
      .filter(_.rearrangementType == RearrangementType.Expansion)
      .collect {
        case Inference(_, expansionPremises, conclusion) if conclusion.conditions.isEmpty =>
          (expansionPremises, conclusion.statement)
      }
      .mapCollect { case (expansionPremises, conclusion) =>
        expansionPremises.toType[DirectPremise]
          .map(_.map(_.statement))
          .map((_, conclusion))
      }
    def findStatementByExpanding(statement: Statement): Iterator[(Conditions, Seq[DirectReference])] = {
      expansions.iterator
        .flatMap { case (expansionPremises, conclusion) =>
          for {
            substitutions <- conclusion.calculateSubstitutions(statement, PartialSubstitutions.empty)
            premisesToFind <- expansionPremises.map(_.applySubstitutions(substitutions.knownSubstitutions)).traverseOption.toSeq
            result <- {
              val initial = Iterator((Conditions.empty, Seq.empty[DirectReference]))
              premisesToFind.foldLeft(initial) { case (acc, premiseToFind) =>
                acc.flatMap { case (conditionsSoFar, referencesSoFar) =>
                  getStatementByRearranging(premiseToFind).map { case (newConditions, newReferences) =>
                    (conditionsSoFar ++ newConditions, referencesSoFar ++ newReferences)
                  }.iterator
                }
              }
            }
          } yield result
        }
    }
    def getStatementByRearranging(statement: Statement): Option[(Conditions, Seq[DirectReference])] = {
      findStatementInFacts(statement).map { case (provenStatement, reference) =>
        (provenStatement.conditions, Seq(reference))
      } orElse findStatementByExpanding(statement).nextOption()
    }
    getStatementByRearranging(assertion).map { case (conditions, references) =>
      AssertionStep(
        ProvenStatement(assertion, conditions),
        Inference.StubSummary("Rearrange"),
        references)
    }
  }

  def findStatementInFacts(statement: Statement): Option[(ProvenStatement, DirectReference)] = {
    provenAssertions.iterator
      .flatMap { case ReferencedAssertion(provenStatement, reference) =>
        getAllSimplifications(provenStatement).toSeq.map((_, reference))
      }
      .find { case (provenStatement, _) =>
          provenStatement.statement == statement
      }
      .map { case (provenStatement, reference) =>
        (provenStatement, reference.copy(html = statement.html))
      }
  }

  def proveAssertionFromTransformedInferences(): Option[TransformedInferenceStep] = {
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
              ReferencedAssertion(ProvenStatement.withNoConditions(premise.statement), DirectReference(index, premise.html))
            } ++ assertions.mapWithIndex { (step, index) =>
              ReferencedAssertion(step.provenStatement, DirectReference(transformedPremises.length + index, step.provenStatement.statement.html))
            }
            Prover(
              statement,
              Set.empty,
              newNonDistinctVariables,
              provenAssertions,
              Nil,
              transformedPremises,
              Nil,
              false
            ).proveAssertionDirectlyFromInferences()
          }.toSeq
        } yield {
          (substitutions, transformedPremises, DetailedProof(proofSteps), proofSteps.last.provenStatement, inference.summary, inference.allowsRearrangement)
        }
      }
      .flatMap { case (substitutions, transformedPremises, transformationProof,  transformedConclusion, inferenceSummary, rearrangementAllowed) =>
        matchPremisesToFacts(transformedPremises, substitutions, rearrangementAllowed)
          .map((transformedConclusion, inferenceSummary, transformationProof, _))
      }
      .flatMap { case (transformedConclusion, inferenceSummary, transformationProof, (matchedPremises, substitutions)) =>
        substitutions.tryResolve().map((transformedConclusion, inferenceSummary, transformationProof, matchedPremises, _))
      }
      .mapCollect { case (transformedConclusion, inferenceSummary, transformationProof, matchedPremises, substitutions) =>
        proveStatement(assertion, transformedConclusion, matchedPremises, substitutions)
          .map(s => TransformedInferenceStep(s, inferenceSummary, transformationProof, matchedPremises.map(_.reference)))
      }
      .nextOption()
  }

  private def matchPremisesToFacts(
    premises: Seq[Premise],
    substitutions: PartialSubstitutions,
    rearrangementAllowed: Boolean
  ): Iterator[(Seq[PremiseMatch], PartialSubstitutions)] = {
    val initial = Iterator((Seq.empty[PremiseMatch], substitutions))
    premises.foldLeft(initial) { case (acc, premise) =>
      acc.flatMap { case (matchedPremisesSoFar, substitutionsSoFar) =>
        matchPremiseToFacts(premise, substitutionsSoFar, rearrangementAllowed).map { case (matchedPremise, newSubstitutions) =>
          (matchedPremisesSoFar :+ matchedPremise, newSubstitutions)
        }
      }
    }
  }

  private def matchPremiseToFacts(
    inferencePremise: Premise,
    substitutionsSoFar: PartialSubstitutions,
    rearrangementAllowed: Boolean
  ): Iterator[(PremiseMatch, PartialSubstitutions)] = {
    inferencePremise match {
      case DirectPremise(premiseStatement) =>
        matchDirectPremiseToFacts(premiseStatement, substitutionsSoFar, rearrangementAllowed)
      case deducedPremise: DeducedPremise =>
        matchDeducedPremiseToFacts(deducedPremise, substitutionsSoFar)
    }
  }

  private def matchDirectPremiseToFacts(
    premiseStatement: Statement,
    substitutionsSoFar: PartialSubstitutions,
    rearrangementAllowed: Boolean
  ): Iterator[(PremiseMatch, PartialSubstitutions)] = {
    provenAssertions.iterator
      .flatMap { case ReferencedAssertion(provenStatement, reference) =>
        if (rearrangementAllowed)
          getAllSimplifications(provenStatement).map { x =>
            (x, reference.copy(html = x.statement.html))
          }
        else
          Seq((provenStatement, reference))
      }
      .flatMap { case (provenStatement, reference) =>
        matchDirectPremiseToFact(premiseStatement, provenStatement, reference, substitutionsSoFar)
      }
  }

  private def matchDirectPremiseToFact(
    premiseStatement: Statement,
    provenStatement: ProvenStatement,
    reference: DirectReference,
    substitutionsSoFar: PartialSubstitutions
  ): Iterator[(PremiseMatch, PartialSubstitutions)] = {
    premiseStatement.calculateSubstitutions(provenStatement.statement, substitutionsSoFar)
      .toIterator
      .map { newSubstitutions =>
        (DirectPremiseMatch(provenStatement, reference), newSubstitutions)
      }
  }

  private def getAllSimplifications(provenStatement: ProvenStatement): Set[ProvenStatement] = {
    def helper(next: Set[ProvenStatement], acc: Set[ProvenStatement]): Set[ProvenStatement] = {
      if (next.isEmpty)
        acc
      else
        helper(next.flatMap(getFirstLevelSimplifications).diff(next ++ acc), next ++ acc)
    }
    helper(Set(provenStatement), Set.empty)
  }

  private def getFirstLevelSimplifications(provenStatement: ProvenStatement): Set[ProvenStatement] = {
    availableInferences
      .filter(_.rearrangementType == RearrangementType.Simplification)
      .collect {
        case Inference(_, Seq(DirectPremise(premiseStatement)), conclusion) if conclusion.conditions.isEmpty =>
          (premiseStatement, conclusion)
      }
      .flatMap { case (premiseStatement, conclusion) =>
        premiseStatement.calculateSubstitutions(provenStatement.statement, PartialSubstitutions.empty)
          .map(_.knownSubstitutions)
          .map((_, conclusion))
      }
      .mapCollect { case (substitutions, conclusion) =>
        conclusion.applySubstitutions(substitutions).map(_.statement)
      }
      .map(ProvenStatement(_, provenStatement.conditions))
      .toSet
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

  private def proveStatement(
    assertion: Statement,
    inferenceConclusion: ProvenStatement,
    matchedPremises: Seq[PremiseMatch],
    substitutions: Substitutions
  ): Option[ProvenStatement] = {
    for {
      substitutedConclusion <- inferenceConclusion.applySubstitutions(substitutions)
      combinedConditions <- (matchedPremises.map(_.provenStatement.conditions) :+ substitutedConclusion.conditions)
        .reduce(_ ++ _)
        .addDistinctVariables(substitutions.distinctVariables)
        .restrictToStatements(premises.flatMap(_.statements) ++ assumptions :+ substitutedConclusion.statement)
        .addDistinctVariables(substitutedConclusion.conditions.arbitraryVariables, assumptions)
      if !nonDistinctVariables.exists { case (first, second) =>
        combinedConditions.distinctVariables.areDistinct(first, second)
      }
      if nonArbitraryVariables.intersect(combinedConditions.arbitraryVariables).isEmpty
    } yield {
      ProvenStatement(assertion, combinedConditions)
    }
  }
}
