package net.prover.model.proof

import net.prover.model.Inference.{DeducedPremise, DirectPremise, Premise, RearrangementType}
import net.prover.model.components.{Statement, StatementVariable, TermVariable, Variable}
import net.prover.model.proof.Proof._
import net.prover.model.{Conditions, Inference, PartialSubstitutions, ProvenStatement, Substitutions, TransformedInference}

case class Prover(
  assertion: Statement,
  nonArbitraryVariables: Set[TermVariable],
  nonDistinctVariables: Set[(TermVariable, Variable)],
  context: ProvingContext,
  debug: Boolean)
{
  import context._

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

  val applicableHints = assertionHints.filter(_.conclusion.statement == assertion)
  lazy val allSimplifiedAssertions = provenAssertions ++ provenAssertions.flatMap(getAllSimplifications)

  def proveAssertion(): Option[StepWithProvenStatement] = {
    proveAssertionUsingHints()
      .orElse(proveAssertionDirectlyFromInferences())
      .orElse(proveAssertionByRearranging())
      .orElse(proveAssertionFromTransformedInferences())
  }

  def proveAssertionUsingHints(): Option[StepWithProvenStatement] = {
    (applicableHints.iterator.flatMap(h => proveUsingInference(h.inference, Some(h.substitutions))) ++
      applicableHints.iterator.flatMap(h => proveUsingElidedInference(h.inference, Some(h.substitutions))) ++
      applicableHints.iterator.flatMap(h => proveAssertionFromTransformedInference(h.inference, Some(h.substitutions)))
    ).nextOption()
  }

  def proveAssertionDirectlyFromInferences(): Option[AssertionStep] = {
    (availableInferences.iterator.flatMap(proveUsingInference(_)) ++
      availableInferences.iterator.flatMap(proveUsingElidedInference(_))
    ).nextOption()
  }

  private def proveUsingInferences(): Option[AssertionStep] = {
    availableInferences.iterator.flatMap(proveUsingInference(_)).nextOption()
  }

  private def proveUsingInference(
    inference: Inference,
    initialSubstitutions: Option[Substitutions] = None
  ): Iterator[AssertionStep] = {
    initialSubstitutions.map(_.toPartial).map(Iterator(_))
      .getOrElse(inference.conclusion.statement.calculateSubstitutions(assertion, PartialSubstitutions.empty).iterator)
      .flatMap { substitutions =>
        matchPremisesToFacts(inference.premises, substitutions, inference.allowsRearrangement)
      }
      .flatMap { case (matchedPremises, substitutions) =>
        substitutions.tryResolve().map((matchedPremises, _))
      }
      .mapCollect { case (matchedPremises, substitutions) =>
        proveStatement(inference.conclusion, matchedPremises.map(_.provenStatement), substitutions)
          .filter(_.statement == assertion)
          .map(p => AssertionStep(p, inference.summary, substitutions, matchedPremises.map(_.reference)))
      }
  }

  private def proveUsingElidedInference(
    inference: Inference,
    initialSubstitutions: Option[Substitutions] = None
  ): Iterator[AssertionStep] = {
    splitPremisesAtElidable(inference.premises).iterator
      .flatMap { case (prePremises, elidablePremise, postPremises) =>
        initialSubstitutions.map(_.toPartial).map(Iterator(_))
          .getOrElse(inference.conclusion.statement.calculateSubstitutions(assertion, PartialSubstitutions.empty).iterator)
          .map {(prePremises, elidablePremise, postPremises, _)}
      }
      .flatMap { case (prePremises, elidablePremise, postPremises, substitutionsAfterConclusion) =>
        matchPremisesToFacts(prePremises, substitutionsAfterConclusion, inference.allowsRearrangement)
          .map { case (prePremiseMatches, substitutionsAfterPrePremises) =>
            (prePremiseMatches, elidablePremise, postPremises, substitutionsAfterPrePremises)
          }
      }
      .flatMap { case (prePremiseMatches, elidablePremise, postPremises, substitutionsAfterPrePremises) =>
        matchPremisesToFacts(postPremises, substitutionsAfterPrePremises, inference.allowsRearrangement)
          .map { case (postPremiseMatches, substitutionsAfterPostPremises) =>
            (prePremiseMatches, elidablePremise, postPremiseMatches, substitutionsAfterPostPremises)
          }
      }
      .flatMap { case (prePremiseMatches, elidablePremise, postPremiseMatches, substitutionsAfterPostPremises) =>
        matchElidablePremise(elidablePremise, substitutionsAfterPostPremises)
          .map { case (elidedPremiseMatch, substitutionsAfterElidedPremise) =>
            ((prePremiseMatches :+ elidedPremiseMatch) ++ postPremiseMatches, substitutionsAfterElidedPremise)
          }
      }
      .flatMap { case (matchedPremises, substitutions) =>
        substitutions.tryResolve().map((matchedPremises, _))
      }
      .mapCollect { case (matchedPremises, substitutions) =>
        proveStatement(inference.conclusion, matchedPremises.map(_.provenStatement), substitutions)
          .filter(_.statement == assertion)
          .map(p => AssertionStep(p, inference.summary, substitutions, matchedPremises.map(_.reference)))
      }
  }

  private def matchElidablePremise(
    premise: DirectPremise,
    premiseSubstitutionsSoFar: PartialSubstitutions
  ): Iterator[(PremiseMatch, PartialSubstitutions)] = {
    availableInferences.iterator
      // Match the premises first, since we don't know what the conclusion should look like
      .flatMap { inference =>
        matchPremisesToFacts(inference.premises, PartialSubstitutions.empty, inference.allowsRearrangement)
          .map(inference -> _)
      }
      // Work out the conclusion by condensing it with the premise
      .mapCollect { case (inference, (matchedPremises, inferenceSubstitutions)) =>
        premise.statement.condense(inference.conclusion.statement, premiseSubstitutionsSoFar, inferenceSubstitutions)
          .map((inference, matchedPremises, _))
      }
      // Now our inference substitutions should be fully determined
      .flatMap { case (inference, matchedPremises, (premiseSubstitutions, inferenceSubstitutions)) =>
        inferenceSubstitutions.tryResolve()
          .map((inference, matchedPremises, premiseSubstitutions, _))
      }
      // So confirm the proof of the inference
      .mapCollect { case (inference, matchedPremises, premiseSubstitutions, inferenceSubstitutions) =>
        proveStatement(inference.conclusion, matchedPremises.map(_.provenStatement), inferenceSubstitutions)
          .map((inference, matchedPremises, premiseSubstitutions, inferenceSubstitutions, _))
      }
      // And finally match the premise to our computed conclusion
      .flatMap { case (inference, matchedPremises, premiseSubstitutions, inferenceSubstitutions, provenConclusion) =>
        matchDirectPremiseToFact(
          premise.statement,
          provenConclusion,
          ElidedReference(inference.summary, inferenceSubstitutions, matchedPremises.map(_.reference)),
          premiseSubstitutions)
      }
  }

  private def splitPremisesAtElidable(premises: Seq[Premise]): Option[(Seq[Premise], DirectPremise, Seq[Premise])] = {
    val (prePremises, elidableAndPostPremises) = premises.span {
      case directPremise: DirectPremise if directPremise.isElidable =>
        false
      case _ =>
        true
    }
    elidableAndPostPremises match {
      case (elidablePremise: DirectPremise) +: postPremises =>
        Some((prePremises, elidablePremise, postPremises))
      case _ =>
        None
    }
  }

  def proveAssertionByRearranging(): Option[RearrangementStep] = {
    val expansions = availableInferences
      .filter(_.rearrangementType == RearrangementType.Expansion)
      .mapCollect { inference =>
        inference.premises.toType[DirectPremise]
          .map(_.map(_.statement))
          .map((inference.summary, _, inference.conclusion))
      }
    def findStatementByExpanding(statement: Statement): Iterator[ReferencedAssertion] = {
      expansions.iterator
        .flatMap { case (summary, infencePremises, conclusion) =>
          for {
            substitutions <- conclusion.statement.calculateSubstitutions(statement, PartialSubstitutions.empty).flatMap(_.tryResolve())
            substitutedPremises <- infencePremises.map(_.applySubstitutions(substitutions)).traverseOption.toSeq
            referencedPremises <- substitutedPremises.map(getStatementByRearranging).traverseOption.toSeq
            provenStatement <- proveStatement(conclusion, referencedPremises.map(_.provenStatement), substitutions)
          } yield ReferencedAssertion(provenStatement, ExpandedReference(summary, substitutions, referencedPremises.map(_.reference)))
        }
    }
    def getStatementByRearranging(statement: Statement): Option[ReferencedAssertion] = {
      findStatementInFacts(statement) orElse findStatementByExpanding(statement).nextOption()
    }
    getStatementByRearranging(assertion).map { case ReferencedAssertion(statement, references) =>
      RearrangementStep(statement, references)
    }
  }

  def findStatementInFacts(statement: Statement): Option[ReferencedAssertion] = {
    allSimplifiedAssertions.find(_.provenStatement.statement == statement)
  }

  def proveAssertionFromTransformedInferences(): Option[TransformedInferenceStep] = {
    availableInferences.iterator
      .mapCollect(proveAssertionFromTransformedInference(_))
      .nextOption()
  }

  def proveAssertionFromTransformedInference(
    inference: Inference,
    initialSubstitutions: Option[Substitutions] = None
  ): Option[TransformedInferenceStep] = {
    inference.premises.toType[DirectPremise].iterator
      .filter { inferencePremises =>
        inferencePremises.forall(_.statement.allVariables.ofType[TermVariable].isEmpty) &&
          inference.conclusion.statement.allVariables.ofType[TermVariable].isEmpty
      }
      .flatMap { inferencePremises =>
        inferenceTransforms.iterator.map(transform => (transform, inferencePremises))
      }
      .flatMap { case (transform, inferencePremises) =>
        val conclusion = inference.conclusion.statement
        transform.transform(inferencePremises, conclusion).iterator
          .map { case (transformedPremises, statementsToProve) =>
            (inferencePremises, transformedPremises, statementsToProve)
          }
      }
      .mapCollect { case (inferencePremises, transformedPremises, statementsToProve) =>
        val transformedConclusion = statementsToProve.last
        val oldVariables = (inferencePremises.map(_.statement) :+ inference.conclusion.statement).flatMap(_.allVariables).toSet
        val newVariables = (transformedPremises.map(_.statement) :+ transformedConclusion).flatMap(_.allVariables).toSet diff oldVariables
        val newNonDistinctVariables = for {
          newVariable <- newVariables.ofType[TermVariable]
          oldVariable <- oldVariables
        } yield newVariable -> oldVariable
        if (newVariables.ofType[StatementVariable].isEmpty) {
          statementsToProve.collectFold[AssertionStep] { case (assertions, statement) =>
            val provenAssertions = transformedPremises.mapWithIndex { (premise, index) =>
              ReferencedAssertion(ProvenStatement.withNoConditions(premise.statement), DirectReference(index))
            } ++ assertions.mapWithIndex { (step, index) =>
              ReferencedAssertion(step.provenStatement, DirectReference(transformedPremises.length + index))
            }
            val localContext = ProvingContext(
              provenAssertions,
              Nil,
              transformedPremises,
              Nil,
              availableInferences,
              Nil,
              assertionHints)
            Prover(statement, Set.empty, newNonDistinctVariables, localContext, debug = false).proveUsingInferences()
          }.map { proofSteps =>
            (TransformedInference(inference, transformedPremises, proofSteps.last.provenStatement), proofSteps)
          }
        } else {
          None
        }
      }
      .flatMap { case (transformedInference, transformationSteps) =>
        (proveUsingInference(transformedInference, initialSubstitutions) ++ proveUsingElidedInference(transformedInference, initialSubstitutions))
          .map((transformedInference, transformationSteps, _))
      }
      .map { case (transformedInference, transformationSteps, assertionStep) =>
        TransformedInferenceStep(
          assertionStep.provenStatement,
          transformedInference.summary,
          transformedInference.premises,
          transformationSteps,
          assertionStep.substitutions,
          assertionStep.references)
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
    allowRearrangement: Boolean
  ): Iterator[(PremiseMatch, PartialSubstitutions)] = {
    val assertions = if (allowRearrangement) allSimplifiedAssertions else provenAssertions
    assertions.iterator.flatMap { case ReferencedAssertion(provenStatement, reference) =>
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

  private def getAllSimplifications(referencedAssertion: ReferencedAssertion): Seq[ReferencedAssertion] = {
    def helper(next: Seq[ReferencedAssertion], acc: Seq[ReferencedAssertion]): Seq[ReferencedAssertion] = {
      if (next.isEmpty)
        acc
      else {
        val newSimplifications = next.flatMap(getNextLevelSimplifications)
        helper(newSimplifications, next ++ acc)
      }
    }
    helper(getNextLevelSimplifications(referencedAssertion), Nil)
  }

  private def getNextLevelSimplifications(referencedAssertion: ReferencedAssertion): Seq[ReferencedAssertion] = {
    availableInferences
      .filter(_.rearrangementType == RearrangementType.Simplification)
      .collect {
        case inference @ Inference(_, Seq(DirectPremise(premiseStatement)), conclusion) if conclusion.conditions.isEmpty =>
          (inference.summary, premiseStatement, conclusion)
      }
      .flatMap { case (summary, premiseStatement, conclusion) =>
        premiseStatement.calculateSubstitutions(referencedAssertion.provenStatement.statement, PartialSubstitutions.empty)
          .map(_.knownSubstitutions)
          .map((summary, _, conclusion))
      }
      .mapCollect { case (summary, substitutions, conclusion) =>
        proveStatement(conclusion, Seq(referencedAssertion.provenStatement), substitutions)
          .map { provenStatement => ReferencedAssertion(
            provenStatement,
            SimplificationReference(provenStatement.statement, summary, substitutions, referencedAssertion.reference))
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

  private def proveStatement(
    inferenceConclusion: ProvenStatement,
    matchedPremises: Seq[ProvenStatement],
    substitutions: Substitutions
  ): Option[ProvenStatement] = {
    for {
      substitutedConclusion <- inferenceConclusion.applySubstitutions(substitutions)
      combinedConditions <- Conditions.combine(
        substitutedConclusion,
        matchedPremises.map(_.conditions),
        premises,
        assumptions,
        substitutions)
      if !nonDistinctVariables.exists { case (first, second) =>
        combinedConditions.distinctVariables.areDistinct(first, second)
      }
      if nonArbitraryVariables.intersect(combinedConditions.arbitraryVariables).isEmpty
    } yield {
      ProvenStatement(substitutedConclusion.statement, combinedConditions)
    }
  }
}
