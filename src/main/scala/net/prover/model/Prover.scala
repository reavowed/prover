package net.prover.model

import jdk.nashorn.internal.runtime.regexp.joni.ast.StateNode
import net.prover.model.DetailedProof._
import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise, RearrangementType}

case class Prover(
  assertion: Statement,
  nonArbitraryVariables: Set[TermVariable],
  nonDistinctVariables: Set[(TermVariable, Variable)],
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
        proveStatement(inference.conclusion, matchedPremises.map(_.provenStatement), substitutions)
          .filter(p => p.statement == assertion)
          .map(p => AssertionStep(p, inference.summary, matchedPremises.map(_.reference)))
      }
      .nextOption()
  }

  def proveAssertionByRearranging(): Option[AssertionStep] = {
    val expansions = availableInferences
      .filter(_.rearrangementType == RearrangementType.Expansion)
      .mapCollect { case Inference(_, expansionPremises, conclusion) =>
        expansionPremises.toType[DirectPremise]
          .map(_.map(_.statement))
          .map((_, conclusion))
      }
    def findStatementByExpanding(statement: Statement): Iterator[(Conditions, Seq[Reference])] = {
      expansions.iterator
        .flatMap { case (expansionPremises, conclusion) =>
          for {
            substitutions <- conclusion.statement.calculateSubstitutions(statement, PartialSubstitutions.empty)
            premisesToFind <- expansionPremises.map(_.applySubstitutions(substitutions.knownSubstitutions)).traverseOption.toSeq
            premiseConditionsAndReferences <- premisesToFind.map(getStatementByRearranging).traverseOption.toSeq
            conditions = conclusion.conditions ++ premiseConditionsAndReferences.map(_._1).foldTogether
            references = premiseConditionsAndReferences.flatMap(_._2)
          } yield (conditions, references)
        }
    }
    def getStatementByRearranging(statement: Statement): Option[(Conditions, Seq[Reference])] = {
      findStatementInFacts(statement).map { case (conditions, reference) =>
        (conditions, Seq(reference))
      } orElse findStatementByExpanding(statement).nextOption()
    }
    getStatementByRearranging(assertion).map { case (conditions, references) =>
      AssertionStep(
        ProvenStatement(assertion, conditions),
        Inference.StubSummary("Rearrange"),
        references)
    }
  }

  def findStatementInFacts(statement: Statement): Option[(Conditions, Reference)] = {
    val directIterator = provenAssertions.iterator
      .collect { case ReferencedAssertion(provenStatement, reference) if provenStatement.statement == statement =>
        (provenStatement.conditions, reference)
      }
    val simplificationsIterator = provenAssertions.iterator
      .flatMap { case ReferencedAssertion(provenStatement, reference) =>
        getAllSimplifications(provenStatement, reference)
      }
      .collect { case (provenStatement, reference) if provenStatement.statement == statement =>
        (provenStatement.conditions, reference)
      }
    (directIterator ++ simplificationsIterator).nextOption()
  }

  def proveAssertionFromTransformedInferences(): Option[TransformedInferenceStep] = {
    availableInferences.iterator
      .mapCollect { inference =>
        inference.premises.toType[DirectPremise].map((inference, _))
      }
      .filter { case (inference, inferencePremises) =>
        inferencePremises.forall(_.statement.allVariables.ofType[TermVariable].isEmpty) &&
          inference.conclusion.statement.allVariables.ofType[TermVariable].isEmpty
      }
      .flatMap { case (inference, inferencePremises) =>
        context.inferenceTransforms.iterator.map(transform => (transform, inference, inferencePremises))
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
        val oldVariables = (inferencePremises.map(_.statement) :+ inference.conclusion.statement).flatMap(_.allVariables).toSet
        val newVariables = (transformedPremises.map(_.statement) :+ transformedConclusion).flatMap(_.allVariables).toSet diff oldVariables
        val newNonDistinctVariables = for {
          newVariable <- newVariables.ofType[TermVariable]
          oldVariable <- oldVariables
        } yield newVariable -> oldVariable
        for {
          substitutions <- transformedConclusion.calculateSubstitutions(assertion, PartialSubstitutions.empty)
          if newVariables.ofType[StatementVariable].isEmpty
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
        proveStatement(transformedConclusion, matchedPremises.map(_.provenStatement), substitutions)
          .filter(_.statement == assertion)
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
          (provenStatement, reference) +: getAllSimplifications(provenStatement, reference)
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
    reference: Reference,
    substitutionsSoFar: PartialSubstitutions
  ): Iterator[(PremiseMatch, PartialSubstitutions)] = {
    premiseStatement.calculateSubstitutions(provenStatement.statement, substitutionsSoFar)
      .toIterator
      .map { newSubstitutions =>
        (DirectPremiseMatch(provenStatement, reference), newSubstitutions)
      }
  }

  private def getAllSimplifications(provenStatement: ProvenStatement, reference: DirectReference): Seq[(ProvenStatement, Reference)] = {
    def helper(next: Seq[Simplification], acc: Seq[Simplification]): Seq[Simplification] = {
      if (next.isEmpty)
        acc
      else {
        val newSimplifications = next.flatMap { s =>
          getNextLevelSimplifications(s.result, s.previous)
        }
        helper(newSimplifications, next ++ acc)
      }
    }
    helper(getNextLevelSimplifications(provenStatement.statement, Nil), Nil).map { simplification =>
      (
        ProvenStatement(simplification.result, provenStatement.conditions),
        SimplifiedReference(reference.index, simplification.result.html, simplification))
    }
  }

  private def getNextLevelSimplifications(statement: Statement, previous: Seq[Statement]): Seq[Simplification] = {
    availableInferences
      .filter(_.rearrangementType == RearrangementType.Simplification)
      .collect {
        case Inference(_, Seq(DirectPremise(premiseStatement)), conclusion) if conclusion.conditions.isEmpty =>
          (premiseStatement, conclusion)
      }
      .flatMap { case (premiseStatement, conclusion) =>
        premiseStatement.calculateSubstitutions(statement, PartialSubstitutions.empty)
          .map(_.knownSubstitutions)
          .map((_, conclusion))
      }
      .mapCollect { case (substitutions, conclusion) =>
        conclusion.applySubstitutions(substitutions)
          .map(_.statement)
          .map(Simplification(_, previous :+ statement))
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

  private def proveStatement(
    inferenceConclusion: ProvenStatement,
    matchedPremises: Seq[ProvenStatement],
    substitutions: Substitutions
  ): Option[ProvenStatement] = {
    for {
      substitutedConclusion <- inferenceConclusion.applySubstitutions(substitutions)
      combinedConditions <- (matchedPremises.map(_.conditions) :+ substitutedConclusion.conditions)
        .reduce(_ ++ _)
        .addDistinctVariables(substitutions.distinctVariables)
        .restrictToStatements(premises.flatMap(_.statements) ++ assumptions :+ substitutedConclusion.statement)
        .addDistinctVariables(substitutedConclusion.conditions.arbitraryVariables, assumptions)
        .map(_.removeImplicitDistinctVariables(substitutedConclusion.statement.implicitDistinctVariables))
      if !nonDistinctVariables.exists { case (first, second) =>
        combinedConditions.distinctVariables.areDistinct(first, second)
      }
      if nonArbitraryVariables.intersect(combinedConditions.arbitraryVariables).isEmpty
    } yield {
      ProvenStatement(substitutedConclusion.statement, combinedConditions)
    }
  }
}
