package net.prover.model.proof

import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.expressions._

import scala.util.Try

case class Prover(
  assertionToProve: Statement,
  reference: Reference.Direct,
  context: ProvingContext,
  debug: Boolean)
{
  import context._

  val applicableHints = assertionHints.filter(_.conclusion == assertionToProve)
  val allSimplifiedFacts = {
    val contractions = referencedFacts.flatMap(getContractions(_))
    val simplifications = (referencedFacts ++ contractions).flatMap(getAllSimplifications)
    referencedFacts ++ contractions ++ simplifications
  }

  lazy val transformations: Seq[Transformation] = transformationStatementDefinitions.mapCollect { statementDefinition =>
    for {
      variableName <- statementDefinition.boundVariableNames.single
    } yield {
      Transformation(statementDefinition, variableName)
    }
  }

  def proveAssertion(): Option[Step.Assertion] = {
    proveAssertionUsingHints()
      .orElse(proveAssertionDirectlyFromInferences())
      .orElse(proveAssertionByRearranging())
  }

  def proveAssertionUsingHints(): Option[Step.Assertion] = {
    applicableHints.iterator.findFirst(h => proveUsingInference(h.inference, Some(h.substitutions)))
  }

  def proveAssertionDirectlyFromInferences(): Option[Step.Assertion] = {
    availableInferences.iterator.findFirst(proveUsingInference(_)) orElse
      availableInferences.iterator.findFirst(proveUsingTransformedInference) orElse
      availableInferences.iterator.findFirst(proveUsingElidedInference(_))
  }

  private def proveUsingInference(
    inference: Inference,
    initialSubstitutions: Option[Substitutions] = None
  ): Option[Step.Assertion] = {
    initialSubstitutions.map(Iterator(_))
      .getOrElse {
        inference.conclusion.calculateSubstitutions(assertionToProve, Substitutions.emptyWithDepth(depth)).iterator
      }
      .flatMap { substitutions =>
        matchPremisesToFacts(inference.premises, substitutions, inference.allowsRearrangement)
      }
      .map { case (premiseReferences, substitutions) =>
        Step.Assertion(
          assertionToProve,
          InferenceApplication.Direct(inference, substitutions, premiseReferences, depth),
          reference,
          isRearrangement = false)
      }
      .headOption
  }

  private def proveUsingTransformedInference(
    inference: Inference
  ): Option[Step.Assertion] = {
    (for {
      transformation <- transformations.iterator
      premiseStatements <- inference.premises.map(_.fact.asOptionalInstanceOf[Fact.Direct].map(_.assertion)).traverseOption.iterator
      (transformedPremises, transformedConclusion, stepsToProve) <- transformation.applyToInference(premiseStatements, inference.conclusion)
      conclusionSubstitutions <- transformedConclusion.calculateSubstitutions(assertionToProve, Substitutions.emptyWithDepth(depth))
      (premiseReferences, premiseSubstitutions) <- matchPremisesToFacts(transformedPremises, conclusionSubstitutions, inference.allowsRearrangement)
      proofOutline = ProofOutline(stepsToProve :+ StepOutline.Assertion(transformedConclusion, None))
      transformationProofAttempt = Try(proofOutline.fillIn(
        transformedPremises,
        availableInferences,
        assertionHints,
        Nil))
      transformationProof <- transformationProofAttempt.toOption
    } yield Step.Assertion(
      assertionToProve,
      InferenceApplication.Transformed(
        inference,
        premiseSubstitutions,
        premiseReferences,
        transformation.statementDefinition,
        transformedPremises,
        transformedConclusion,
        transformationProof.steps,
        depth),
      reference,
      isRearrangement = false)
    ).headOption
  }

  private def proveUsingElidedInference(
    inference: Inference,
    initialSubstitutions: Option[Substitutions] = None
  ): Option[Step.Assertion] = {
    (for {
      (prePremises, elidablePremise, postPremises) <- splitPremisesAtElidable(inference.premises).iterator
      substitutionsAfterConclusion <- initialSubstitutions.map(Iterator(_))
        .getOrElse(inference.conclusion.calculateSubstitutions(assertionToProve, Substitutions.emptyWithDepth(depth)).iterator)
      (prePremiseReferences, substitutionsAfterPrePremises) <- matchPremisesToFacts(
        prePremises,
        substitutionsAfterConclusion,
        inference.allowsRearrangement)
      (postPremiseReferences, substitutionsAfterPostPremises) <- matchPremisesToFacts(
        postPremises,
        substitutionsAfterPrePremises,
        inference.allowsRearrangement)
      (elidedPremiseReference, substitutionsAfterElidedPremise) <- matchElidablePremise(
        elidablePremise,
        substitutionsAfterPostPremises)
    } yield Step.Assertion(
        assertionToProve,
        InferenceApplication.Direct(
          inference,
          substitutionsAfterElidedPremise,
          (prePremiseReferences :+ elidedPremiseReference) ++ postPremiseReferences,
          depth),
        reference,
        isRearrangement = false)
    ).headOption
  }

  private def matchElidablePremise(
    premise: Statement,
    premiseSubstitutionsSoFar: Substitutions
  ): Iterator[(Reference, Substitutions)] = {
    for {
      inference <- availableInferences.iterator
      // Match the premises first, since we don't know what the conclusion should look like
      (premiseReferences, inferenceSubstitutionsAfterPremises) <- matchPremisesToFacts(
        inference.premises,
        Substitutions.emptyWithDepth(depth),
        inference.allowsRearrangement)
      // Work out the substitutions by condensing the conclusion with the premise
      (premiseSubstitutions, inferenceSubstitutions) <- premise.condense(
        inference.conclusion,
        premiseSubstitutionsSoFar,
        inferenceSubstitutionsAfterPremises
      ).iterator
      provenConclusion <- inference.conclusion.applySubstitutions(inferenceSubstitutions).iterator
      finalSubstitutions <- premise.calculateSubstitutions(provenConclusion, premiseSubstitutions).iterator
    } yield Reference.Elided(InferenceApplication.Direct(inference, inferenceSubstitutions, premiseReferences, depth)) -> finalSubstitutions
  }

  private def splitPremisesAtElidable(premises: Seq[Premise]): Option[(Seq[Premise], Statement, Seq[Premise])] = {
    val (prePremises, elidableAndPostPremises) = premises.span {
      case premise if premise.isElidable =>
        false
      case _ =>
        true
    }
    elidableAndPostPremises match {
      case Premise(Fact.Direct(premiseStatement), _) +: postPremises =>
        Some((prePremises, premiseStatement, postPremises))
      case _ =>
        None
    }
  }

  def proveAssertionByRearranging(): Option[Step.Assertion] = {
    val expansions = availableInferences
      .filter(_.rearrangementType == RearrangementType.Expansion)
      .mapCollect { inference =>
        inference.premises.map(_.fact).toType[Fact.Direct]
          .map(_.map(_.assertion))
          .map((inference, _))
      }
    def findAssertionByExpanding(assertion: Statement): Option[InferenceApplication] = {
      expansions.iterator
        .findFirst { case (inference, inferencePremises) =>
          (
            for {
              substitutions <- inference.conclusion.calculateSubstitutions(assertion, Substitutions.emptyWithDepth(depth))
              substitutedPremises <- inferencePremises.map(_.applySubstitutions(substitutions)).traverseOption.toSeq
              premiseReferences <- substitutedPremises.map(getAssertionByRearranging).traverseOption.toSeq
              if inference.conclusion.applySubstitutions(substitutions).contains(assertion)
            } yield InferenceApplication.Direct(inference, substitutions, premiseReferences, depth)
          ).headOption
        }
    }
    def getAssertionByRearranging(assertion: Statement): Option[Reference] = {
      findAssertionInFacts(assertion) orElse findAssertionByExpanding(assertion).map(Reference.Expansion)
    }
    findAssertionByExpanding(assertionToProve).map { inferenceApplication =>
      Step.Assertion(assertionToProve, inferenceApplication, reference, isRearrangement = true)
    }
  }

  def findAssertionInFacts(assertion: Statement): Option[Reference] = {
    allSimplifiedFacts.find(_.fact == Fact.Direct(assertion)).map(_.reference)
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
        helper(newSimplifications, acc ++ newSimplifications)
      }
    }
    helper(Seq(referencedFact), Nil)
  }

  private def getContractions(referencedFact: ReferencedFact, level: Int = 0, additionalDepth: Int = 0): Seq[ReferencedFact] = {
    val nextLevelContractions = for {
      (childFact, moreDepth, updater) <- referencedFact.childDetails.toSeq
      contractedChild <- getContractions(childFact, level + 1, additionalDepth + moreDepth)
    } yield updater(contractedChild)
    (referencedFact +: nextLevelContractions).flatMap(getTopLevelContractions(_, level, additionalDepth)) ++ nextLevelContractions
  }

  def getTopLevelContractions(referencedFact: ReferencedFact, level: Int, additionalDepth: Int): Seq[ReferencedFact] = {
    availableInferences
      .filter(_.rearrangementType == RearrangementType.Contraction)
      .collect {
        case inference @ Inference(_, Seq(Premise(premiseFact, _)), _) =>
          (inference, premiseFact)
      }
      .flatMap { case (inference, premiseFact) =>
        premiseFact.calculateSubstitutions(referencedFact.fact, Substitutions.emptyWithDepth(depth + additionalDepth))
          .map((inference, _))
      }
      .mapCollect { case (inference, substitutions) =>
        inference.conclusion.applySubstitutions(substitutions)
          .map((inference, substitutions, _))
      }
      .map { case (inference, substitutions, conclusion) =>
        ReferencedFact(
          Fact.Direct(conclusion),
          Reference.Contraction(inference, substitutions, referencedFact.reference, level, additionalDepth, depth + additionalDepth))
      }
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
            premiseStatement.findComponentPath(inference.conclusion)
              .map((inference, premiseStatement, _))
          }
          .flatMap { case (inference, premiseStatement, simplificationPath) =>
            premiseStatement.calculateSubstitutions(statement, Substitutions.emptyWithDepth(depth))
              .map((inference, simplificationPath, _))
          }
          .mapCollect { case (inference, simplificationPath, substitutions) =>
            inference.conclusion.applySubstitutions(substitutions)
              .map((inference, simplificationPath, substitutions, _))
          }
          .map { case (inference, simplificationPath, substitutions, conclusion) =>
            ReferencedFact(
              Fact.Direct(conclusion),
              Reference.Simplification(inference, substitutions, factReference, simplificationPath, depth))
          }
      case _ =>
        Nil
    }
  }
}
