package net.prover.model.proof

import net.prover.model.Inference.RearrangementType
import net.prover.model.components.Statement
import net.prover.model.proof.Proof._
import net.prover.model.{Inference, Premise, Substitutions}

case class Prover(
  assertion: Statement,
  context: ProvingContext,
  debug: Boolean)
{
  import context._

  sealed trait PremiseMatch {
    def reference: Reference
  }
  case class DirectPremiseMatch(
    statement: Statement,
    reference: Reference)
    extends PremiseMatch
  case class DeducedPremiseMatch(
    antecedent: Statement,
    consequent: Statement,
    reference: Reference)
    extends PremiseMatch

  val applicableHints = assertionHints.filter(_.conclusion == assertion)
  lazy val allSimplifiedAssertions = provenAssertions ++ provenAssertions.flatMap(getAllSimplifications)

  def proveAssertion(): Option[StepWithProvenStatement] = {
    proveAssertionUsingHints()
      .orElse(proveAssertionDirectlyFromInferences())
      .orElse(proveAssertionByRearranging())
  }

  def proveAssertionUsingHints(): Option[StepWithProvenStatement] = {
    (applicableHints.iterator.flatMap(h => proveUsingInference(h.inference, Some(h.substitutions))) ++
      applicableHints.iterator.flatMap(h => proveUsingElidedInference(h.inference, Some(h.substitutions)))
    ).nextOption()
  }

  def proveAssertionDirectlyFromInferences(): Option[AssertionStep] = {
    (availableInferences.iterator.flatMap(proveUsingInference(_)) ++
      availableInferences.iterator.flatMap(proveUsingElidedInference(_))
    ).nextOption()
  }

  private def proveUsingInference(
    inference: Inference,
    initialSubstitutions: Option[Substitutions] = None
  ): Iterator[AssertionStep] = {
    initialSubstitutions.map(Iterator(_))
      .getOrElse {
        inference.conclusion.calculateSubstitutions(assertion, Substitutions.empty).iterator
      }
      .flatMap { substitutions =>
        matchPremisesToFacts(inference.premises, substitutions, inference.allowsRearrangement)
      }
      .mapCollect { case (matchedPremises, substitutions) =>
        inference.conclusion.applySubstitutions(substitutions)
          .filter(_ == assertion)
          .map(p => AssertionStep(p, inference.summary, substitutions, matchedPremises.map(_.reference)))
      }
  }

  private def proveUsingElidedInference(
    inference: Inference,
    initialSubstitutions: Option[Substitutions] = None
  ): Iterator[AssertionStep] = {
    splitPremisesAtElidable(inference.premises).iterator
      .flatMap { case (prePremises, elidablePremise, postPremises) =>
        initialSubstitutions.map(Iterator(_))
          .getOrElse(inference.conclusion.calculateSubstitutions(assertion, Substitutions.empty).iterator)
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
        matchElidablePremise(elidablePremise.statement, substitutionsAfterPostPremises)
          .map { case (elidedPremiseMatch, substitutionsAfterElidedPremise) =>
            ((prePremiseMatches :+ elidedPremiseMatch) ++ postPremiseMatches, substitutionsAfterElidedPremise)
          }
      }
      .mapCollect { case (matchedPremises, substitutions) =>
        inference.conclusion.applySubstitutions(substitutions)
          .filter(_ == assertion)
          .map(p => AssertionStep(p, inference.summary, substitutions, matchedPremises.map(_.reference)))
      }
  }

  private def matchElidablePremise(
    premise: Statement,
    premiseSubstitutionsSoFar: Substitutions
  ): Iterator[(PremiseMatch, Substitutions)] = {
    availableInferences.iterator
      // Match the premises first, since we don't know what the conclusion should look like
      .flatMap { inference =>
        matchPremisesToFacts(inference.premises, Substitutions.empty, inference.allowsRearrangement)
          .map(inference -> _)
      }
      // Work out the substitutions by condensing the conclusion with the premise
      .mapCollect { case (inference, (matchedPremises, inferenceSubstitutions)) =>
        premise.condense(inference.conclusion, premiseSubstitutionsSoFar, inferenceSubstitutions)
          .map((inference, matchedPremises, _))
      }
      // Confirm the final conclusion of the inference
      .mapCollect { case (inference, matchedPremises, (premiseSubstitutions, inferenceSubstitutions)) =>
        inference.conclusion.applySubstitutions(inferenceSubstitutions)
          .map((inference, matchedPremises, premiseSubstitutions, inferenceSubstitutions, _))
      }
      // And finally match the premise to our computed conclusion
      .flatMap { case (inference, matchedPremises, premiseSubstitutions, inferenceSubstitutions, provenConclusion) =>
        matchDirectPremiseToFact(
          premise,
          provenConclusion,
          ElidedReference(inference.summary, inferenceSubstitutions, matchedPremises.map(_.reference)),
          premiseSubstitutions)
      }
  }

  private def splitPremisesAtElidable(premises: Seq[Premise]): Option[(Seq[Premise], Premise.DirectPremise, Seq[Premise])] = {
    val (prePremises, elidableAndPostPremises) = premises.span {
      case directPremise: Premise.DirectPremise if directPremise.isElidable =>
        false
      case _ =>
        true
    }
    elidableAndPostPremises match {
      case (elidablePremise: Premise.DirectPremise) +: postPremises =>
        Some((prePremises, elidablePremise, postPremises))
      case _ =>
        None
    }
  }

  def proveAssertionByRearranging(): Option[RearrangementStep] = {
    val expansions = availableInferences
      .filter(_.rearrangementType == RearrangementType.Expansion)
      .mapCollect { inference =>
        inference.premises.toType[Premise.DirectPremise]
          .map(_.map(_.statement))
          .map((inference.summary, _, inference.conclusion))
      }
    def findStatementByExpanding(statement: Statement): Iterator[ReferencedAssertion] = {
      expansions.iterator
        .flatMap { case (summary, infencePremises, conclusion) =>
          for {
            substitutions <- conclusion.calculateSubstitutions(statement, Substitutions.empty)
            substitutedPremises <- infencePremises.map(_.applySubstitutions(substitutions)).traverseOption.toSeq
            x = substitutedPremises.map(getStatementByRearranging)
            referencedPremises <- x.traverseOption.toSeq
            if conclusion.applySubstitutions(substitutions).contains(statement)
          } yield ReferencedAssertion(statement, ExpandedReference(summary, substitutions, referencedPremises.map(_.reference)))
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
    allSimplifiedAssertions.find(_.statement == statement)
  }

  private def matchPremisesToFacts(
    premises: Seq[Premise],
    substitutions: Substitutions,
    rearrangementAllowed: Boolean
  ): Iterator[(Seq[PremiseMatch], Substitutions)] = {
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
    substitutionsSoFar: Substitutions,
    rearrangementAllowed: Boolean
  ): Iterator[(PremiseMatch, Substitutions)] = {
    inferencePremise match {
      case Premise.DirectPremise(premiseStatement) =>
        matchDirectPremiseToFacts(premiseStatement, substitutionsSoFar, rearrangementAllowed)
      case deducedPremise: Premise.DeducedPremise =>
        matchDeducedPremiseToFacts(deducedPremise, substitutionsSoFar)
    }
  }

  private def matchDirectPremiseToFacts(
    premiseStatement: Statement,
    substitutionsSoFar: Substitutions,
    allowRearrangement: Boolean
  ): Iterator[(PremiseMatch, Substitutions)] = {
    val assertions = if (allowRearrangement) allSimplifiedAssertions else provenAssertions
    assertions.iterator.flatMap { case ReferencedAssertion(provenStatement, reference) =>
      matchDirectPremiseToFact(premiseStatement, provenStatement, reference, substitutionsSoFar)
    }
  }

  private def matchDirectPremiseToFact(
    premiseStatement: Statement,
    knownStatement: Statement,
    reference: Reference,
    substitutionsSoFar: Substitutions
  ): Iterator[(PremiseMatch, Substitutions)] = {
    premiseStatement.calculateSubstitutions(knownStatement, substitutionsSoFar)
      .toIterator
      .map { newSubstitutions =>
        (DirectPremiseMatch(knownStatement, reference), newSubstitutions)
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
        case inference @ Inference(_, Seq(Premise.DirectPremise(premiseStatement)), conclusion) =>
          (inference.summary, premiseStatement, conclusion)
      }
      .flatMap { case (summary, premiseStatement, conclusion) =>
        premiseStatement.calculateSubstitutions(referencedAssertion.statement, Substitutions.empty)
          .map((summary, _, conclusion))
      }
      .mapCollect { case (summary, substitutions, conclusion) =>
        conclusion.applySubstitutions(substitutions)
          .map { provenStatement => ReferencedAssertion(
            provenStatement,
            SimplificationReference(provenStatement, summary, substitutions, referencedAssertion.reference))
          }
      }
  }

  private def matchDeducedPremiseToFacts(
    inferencePremise: Premise.DeducedPremise,
    substitutionsSoFar: Substitutions
  ): Iterator[(PremiseMatch, Substitutions)] = {
    provenDeductions.iterator.flatMap { case ReferencedDeduction(antecedent, consequent, reference) =>
      inferencePremise.antecedent.calculateSubstitutions(antecedent, substitutionsSoFar)
        .flatMap(inferencePremise.consequent.calculateSubstitutions(consequent, _))
        .map((DeducedPremiseMatch(antecedent, consequent, reference), _))
    }
  }
}
