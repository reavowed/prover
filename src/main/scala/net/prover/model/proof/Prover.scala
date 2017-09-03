package net.prover.model.proof

import net.prover.model.Inference.RearrangementType
import net.prover.model.components.Statement
import net.prover.model.{Inference, Premise, Substitutions}

case class Prover(
  assertion: Statement,
  reference: Reference.Direct,
  context: ProvingContext,
  debug: Boolean)
{
  import context._

  val applicableHints = assertionHints.filter(_.conclusion == assertion)
  lazy val allSimplifiedFacts = referencedFacts ++ referencedFacts.flatMap(getAllSimplifications)

  def proveAssertion(): Option[Step.Assertion] = {
    proveAssertionUsingHints()
      .orElse(proveAssertionDirectlyFromInferences())
      .orElse(proveAssertionByRearranging())
  }

  def proveAssertionUsingHints(): Option[Step.Assertion] = {
    applicableHints.iterator.flatMap(h => proveUsingInference(h.inference, Some(h.substitutions))).nextOption()
  }

  def proveAssertionDirectlyFromInferences(): Option[Step.Assertion] = {
    availableInferences.iterator.flatMap(proveUsingInference(_)).nextOption()
  }

  private def proveUsingInference(
    inference: Inference,
    initialSubstitutions: Option[Substitutions] = None
  ): Iterator[Step.Assertion] = {
    initialSubstitutions.map(Iterator(_))
      .getOrElse {
        inference.conclusion.calculateSubstitutions(assertion, Substitutions.empty).iterator
      }
      .flatMap { substitutions =>
        matchPremisesToFacts(inference.premises, substitutions, inference.allowsRearrangement)
      }
      .mapCollect { case (premiseReferences, substitutions) =>
        makeAssertion(inference, substitutions, premiseReferences)
      }
  }

//  private def proveUsingElidedInference(
//    inference: Inference,
//    initialSubstitutions: Option[Substitutions] = None
//  ): Iterator[Step.Assertion] = {
//    splitPremisesAtElidable(inference.premises).iterator
//      .flatMap { case (prePremises, elidablePremise, postPremises) =>
//        initialSubstitutions.map(Iterator(_))
//          .getOrElse(inference.conclusion.calculateSubstitutions(assertion, Substitutions.empty).iterator)
//          .map {(prePremises, elidablePremise, postPremises, _)}
//      }
//      .flatMap { case (prePremises, elidablePremise, postPremises, substitutionsAfterConclusion) =>
//        matchPremisesToFacts(prePremises, substitutionsAfterConclusion, inference.allowsRearrangement)
//          .map { case (prePremiseMatches, substitutionsAfterPrePremises) =>
//            (prePremiseMatches, elidablePremise, postPremises, substitutionsAfterPrePremises)
//          }
//      }
//      .flatMap { case (prePremiseReferences, elidablePremise, postPremises, substitutionsAfterPrePremises) =>
//        matchPremisesToFacts(postPremises, substitutionsAfterPrePremises, inference.allowsRearrangement)
//          .map { case (postPremiseReferences, substitutionsAfterPostPremises) =>
//            (prePremiseReferences, elidablePremise, postPremiseReferences, substitutionsAfterPostPremises)
//          }
//      }
//      .flatMap { case (prePremiseReferences, elidablePremise, postPremiseReferences, substitutionsAfterPostPremises) =>
//        matchElidablePremise(elidablePremise, substitutionsAfterPostPremises)
//          .map { case (elidedPremiseReference, substitutionsAfterElidedPremise) =>
//            ((prePremiseReferences :+ elidedPremiseReference) ++ postPremiseReferences, substitutionsAfterElidedPremise)
//          }
//      }
//      .mapCollect { case (premiseReferences, substitutions) =>
//        makeAssertion(inference, substitutions, premiseReferences)
//      }
//  }

  private def makeAssertion(inference: Inference, substitutions: Substitutions, references: Seq[Reference]): Option[Step.Assertion] = {
    Option(substitutions)
      .filter(s => inference.conclusion.applySubstitutions(s).contains(assertion))
      .flatMap(inference.specifySubstitutions)
      .map(s => Step.Assertion(assertion, InferenceApplication(inference.summary, s, references), reference, isRearrangement = false))
  }

//  private def matchElidablePremise(
//    premise: Statement,
//    premiseSubstitutionsSoFar: Substitutions
//  ): Iterator[(Reference, Substitutions)] = {
//    availableInferences.iterator
//      // Match the premises first, since we don't know what the conclusion should look like
//      .flatMap { inference =>
//        matchPremisesToFacts(inference.premises, Substitutions.empty, inference.allowsRearrangement)
//          .map(inference -> _)
//      }
//      // Work out the substitutions by condensing the conclusion with the premise
//      .mapCollect { case (inference, (premiseReferences, inferenceSubstitutions)) =>
//        premise.condense(inference.conclusion, premiseSubstitutionsSoFar, inferenceSubstitutions)
//          .map((inference, premiseReferences, _))
//      }
//      // Confirm the final conclusion of the inference
//      .mapCollect { case (inference, premiseReferences, (premiseSubstitutions, inferenceSubstitutions)) =>
//        inference.conclusion.applySubstitutions(inferenceSubstitutions)
//          .map((inference, premiseReferences, premiseSubstitutions, inferenceSubstitutions, _))
//      }
//      .mapCollect { case (inference, premiseReferences, premiseSubstitutions, inferenceSubstitutions, provenConclusion) =>
//        inference.specifySubstitutions(inferenceSubstitutions)
//          .map((inference, premiseReferences, premiseSubstitutions, _, provenConclusion))
//      }
//      // And finally match the premise to our computed conclusion
//      .flatMap { case (inference, premiseReferences, premiseSubstitutions, inferenceSubstitutions, provenConclusion) =>
//        premise.calculateSubstitutions(provenConclusion, premiseSubstitutions).iterator
//          .map { substitutions =>
//            Reference.Elided(InferenceApplication(inference.summary, inferenceSubstitutions, premiseReferences)) -> substitutions
//          }
//      }
//  }
//
//  private def splitPremisesAtElidable(premises: Seq[Premise]): Option[(Seq[Premise], Statement, Seq[Premise])] = {
//    val (prePremises, elidableAndPostPremises) = premises.span {
//      case premise if premise.isElidable =>
//        false
//      case _ =>
//        true
//    }
//    elidableAndPostPremises match {
//      case Premise(Fact.Direct(premiseStatement), _) +: postPremises =>
//        Some((prePremises, premiseStatement, postPremises))
//      case _ =>
//        None
//    }
//  }

  def proveAssertionByRearranging(): Option[Step.Assertion] = {
    val expansions = availableInferences
      .filter(_.rearrangementType == RearrangementType.Expansion)
      .mapCollect { inference =>
        inference.premises.map(_.fact).toType[Fact.Direct]
          .map(_.map(_.statement))
          .map((inference, _))
      }
    def findStatementByExpanding(statement: Statement): Option[InferenceApplication] = {
      expansions.iterator
        .flatMap { case (inference, inferencePremises) =>
          for {
            substitutions <- inference.conclusion.calculateSubstitutions(statement, Substitutions.empty)
            substitutedPremises <- inferencePremises.map(_.applySubstitutions(substitutions)).traverseOption.toSeq
            premiseReferences <- substitutedPremises.map(getStatementByRearranging).traverseOption.toSeq
            if inference.conclusion.applySubstitutions(substitutions).contains(statement)
            inferenceSubstitutions <- inference.specifySubstitutions(substitutions)
          } yield InferenceApplication(inference.summary, inferenceSubstitutions, premiseReferences)
        }
        .nextOption()
    }
    def getStatementByRearranging(statement: Statement): Option[Reference] = {
      findStatementInFacts(statement) orElse findStatementByExpanding(statement).map(Reference.Expansion(_))
    }
    findStatementByExpanding(assertion).map { inferenceApplication =>
      Step.Assertion(assertion, inferenceApplication, reference, isRearrangement = true)
    }
  }

  def findStatementInFacts(statement: Statement): Option[Reference] = {
    allSimplifiedFacts.find(_.fact == Fact.Direct(statement)).map(_.reference)
  }

  private def matchPremisesToFacts(
    premises: Seq[Premise],
    substitutions: Substitutions,
    rearrangementAllowed: Boolean
  ): Iterator[(Seq[Reference], Substitutions)] = {
    val initial = Iterator((Seq.empty[Reference], substitutions))
    premises.foldLeft(initial) { case (acc, premise) =>
      acc.flatMap { case (referencesSoFar, substitutionsSoFar) =>
        matchPremiseToFacts(premise, substitutionsSoFar, rearrangementAllowed).map { case (premiseReference, newSubstitutions) =>
          (referencesSoFar :+ premiseReference, newSubstitutions)
        }
      }
    }
  }

  private def matchPremiseToFacts(
    premise: Premise,
    substitutionsSoFar: Substitutions,
    allowRearrangement: Boolean
  ): Iterator[(Reference, Substitutions)] = {
    val facts = if (allowRearrangement) allSimplifiedFacts else referencedFacts
    facts.iterator.flatMap { case ReferencedFact(fact, factReference) =>
      matchPremiseToFact(premise.fact, fact, factReference, substitutionsSoFar)
    }
  }

  private def matchPremiseToFact(
    premiseFact: Fact,
    knownFact: Fact,
    factReference: Reference,
    substitutionsSoFar: Substitutions
  ): Iterator[(Reference, Substitutions)] = {
    premiseFact.calculateSubstitutions(knownFact, substitutionsSoFar)
      .toIterator
      .map { newSubstitutions =>
        (factReference, newSubstitutions)
      }
  }

  private def getAllSimplifications(referencedFact: ReferencedFact): Seq[ReferencedFact] = {
    def helper(next: Seq[ReferencedFact], acc: Seq[ReferencedFact]): Seq[ReferencedFact] = {
      if (next.isEmpty)
        acc
      else {
        val newSimplifications = next.flatMap(getNextLevelSimplifications)
        helper(newSimplifications, next ++ acc)
      }
    }
    helper(getNextLevelSimplifications(referencedFact), Nil)
  }

  private def getNextLevelSimplifications(referencedFact: ReferencedFact): Seq[ReferencedFact] = {
    referencedFact match {
      case ReferencedFact(Fact.Direct(statement), factReference) =>
        availableInferences
          .filter(_.rearrangementType == RearrangementType.Simplification)
          .collect {
            case inference @ Inference(_, Seq(Premise(Fact.Direct(premiseStatement), _)), _) =>
              (inference, premiseStatement)
          }
          .flatMap { case (inference, premiseStatement) =>
            premiseStatement.findSubcomponent(inference.conclusion)
              .map((inference, premiseStatement, _))
          }
          .flatMap { case (inference, premiseStatement, simplificationPath) =>
            premiseStatement.calculateSubstitutions(statement, Substitutions.empty)
              .map((inference, simplificationPath, _))
          }
          .mapCollect { case (inference, simplificationPath, substitutions) =>
            inference.conclusion.applySubstitutions(substitutions)
              .map((inference, simplificationPath, substitutions, _))
          }
          .mapCollect { case (inference, simplificationPath, substitutions, conclusion) =>
            inference.specifySubstitutions(substitutions)
              .map((inference, simplificationPath, _, conclusion))
          }
          .map { case (inference, simplificationPath, substitutions, conclusion) =>
            ReferencedFact(
              Fact.Direct(conclusion),
              Reference.Simplification(inference.summary, substitutions, factReference, simplificationPath))
          }
      case _ =>
        Nil
    }
  }
}
