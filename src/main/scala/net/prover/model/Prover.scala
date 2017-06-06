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

  def proveAssertion(): AssertionStep = {
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
        matchPremisesToFacts(inference, substitutions).map(inference -> _)
      }
      .flatMap { case (inference, (matchedPremises, substitutions)) =>
        substitutions.tryResolve().map((inference, matchedPremises, _))
      }
      .mapCollect { case (inference, matchedPremises, substitutions) =>
        if (debug) {
          println(inference)
          println(substitutions)
          println(inference.conclusion)
          inference.premises.foreach { p =>
            println(p.applySubstitutions(substitutions))
          }
          println(inference.conclusion.applySubstitutions(substitutions))
          println()
        }
        makeAssertionStep(assertion, inference, matchedPremises, substitutions)
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
      AssertionStep(ProvenStatement(assertion, conditions), InferenceSummary("", "Rearrange"), references)
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
  }

  def proveAssertionFromTransformedInferences(): Option[AssertionStep] = {
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
          val transformedInference = DerivedInference(
            Some(inference.id),
            inference.name,
            transformedPremises,
            proofSteps.last.provenStatement,
            inference.rearrangementType,
            inference.allowsRearrangement)
          (substitutions, transformedInference)
        }
      }
      .flatMap { case (substitutions, transformedInference) =>
        matchPremisesToFacts(transformedInference, substitutions).map((transformedInference, _))
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
    inference: Inference,
    substitutions: PartialSubstitutions
  ): Iterator[(Seq[PremiseMatch], PartialSubstitutions)] = {
    val initial = Iterator((Seq.empty[PremiseMatch], substitutions))
    inference.premises.foldLeft(initial) { case (acc, premise) =>
      acc.flatMap { case (matchedPremisesSoFar, substitutionsSoFar) =>
        matchPremiseToFacts(premise, inference, substitutionsSoFar).map { case (matchedPremise, newSubstitutions) =>
          (matchedPremisesSoFar :+ matchedPremise, newSubstitutions)
        }
      }
    }
  }

  private def matchPremiseToFacts(
    inferencePremise: Premise,
    inference: Inference,
    substitutionsSoFar: PartialSubstitutions
  ): Iterator[(PremiseMatch, PartialSubstitutions)] = {
    inferencePremise match {
      case DirectPremise(premiseStatement) =>
        matchDirectPremiseToFacts(premiseStatement, inference, substitutionsSoFar)
      case deducedPremise: DeducedPremise =>
        matchDeducedPremiseToFacts(deducedPremise, substitutionsSoFar)
    }
  }

  private def matchDirectPremiseToFacts(
    premiseStatement: Statement,
    inference: Inference,
    substitutionsSoFar: PartialSubstitutions
  ): Iterator[(PremiseMatch, PartialSubstitutions)] = {
    provenAssertions.iterator
      .flatMap { case ReferencedAssertion(provenStatement, reference) =>
        if (inference.allowsRearrangement)
          getAllSimplifications(provenStatement).map((_, reference))
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
      if nonArbitraryVariables.intersect(combinedConditions.arbitraryVariables).isEmpty
      provenStatement = ProvenStatement(assertion, combinedConditions)
    } yield {
      AssertionStep(provenStatement, InferenceSummary(inference.id, inference.name), matchedPremises.map(_.reference))
    }
  }
}
