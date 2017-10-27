package net.prover.model.proof

import net.prover.model.Inference.RearrangementType
import net.prover.model.expressions.{ArgumentList, Statement}
import net.prover.model.{Inference, Premise, Substitutions}

import scala.util.Try

case class InferenceProver(
  inference: Inference,
  referencedStatements: Seq[ReferencedStatement])(
  implicit provingContext: ProvingContext)
{
  def proveDirectly(
    assertionToProve: Statement,
    reference: Reference.Direct,
    initialSubstitutionsOption: Option[Substitutions] = None
  ): Option[Step.Assertion] = {
    (for {
      initialSubstitutions <- Iterator(initialSubstitutionsOption.getOrElse(provingContext.defaultSubstitutions))
      conclusionSubstitutions <- inference.conclusion.calculateSubstitutions(assertionToProve, initialSubstitutions, Nil)
      (premiseReferences, premiseSubstitutions) <- matchPremisesToProvenStatements(
        inference.premises,
        conclusionSubstitutions)
    } yield Step.Assertion(
      assertionToProve,
      InferenceApplication.Direct(inference, premiseSubstitutions, premiseReferences, provingContext.depth),
      reference,
      isRearrangement = false)
    ).headOption
  }

  def proveWithTransformation(
    assertionToProve: Statement,
    reference: Reference.Direct
  ): Option[Step.Assertion] = {
    (for {
      transformation <- provingContext.scopingStatement.flatMap(Transformation.apply).toSeq
      if provingContext.allowTransformations
      (transformedPremises, transformedConclusion, stepsToProve) <- transformation.applyToInference(inference.premises, inference.conclusion)
      conclusionSubstitutions <- transformedConclusion.calculateSubstitutions(assertionToProve, provingContext.defaultSubstitutions, Nil)
      (premiseReferences, premiseSubstitutions) <- matchPremisesToProvenStatements(
        transformedPremises,
        conclusionSubstitutions)
      transformationProofAttempt = Try(ProofOutline(stepsToProve)
        .fillIn(provingContext.resetWithPremises(transformedPremises).copy(allowTransformations = false)))
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
        provingContext.depth),
      reference,
      isRearrangement = false)
    ).headOption
  }

  def proveByEliding(
    assertionToProve: Statement,
    reference: Reference.Direct,
    initialSubstitutionsOption: Option[Substitutions] = None
  ): Option[Step.Assertion] = {
    val initialSubstitutions = initialSubstitutionsOption.getOrElse(provingContext.defaultSubstitutions)
    (for {
      (prePremises, elidablePremise, postPremises) <- splitPremisesAtElidable(inference.premises).iterator
      substitutionsAfterConclusion <- inference.conclusion.calculateSubstitutions(assertionToProve, initialSubstitutions, Nil)
      (prePremiseReferences, substitutionsAfterPrePremises) <- matchPremisesToProvenStatements(
        prePremises,
        substitutionsAfterConclusion)
      (postPremiseReferences, substitutionsAfterPostPremises) <- matchPremisesToProvenStatements(
        postPremises,
        substitutionsAfterPrePremises)
      (elidedPremiseReference, substitutionsAfterElidedPremise) <- matchElidablePremise(
        elidablePremise,
        substitutionsAfterPostPremises)
    } yield Step.Assertion(
      assertionToProve,
      InferenceApplication.Direct(
        inference,
        substitutionsAfterElidedPremise,
        (prePremiseReferences :+ elidedPremiseReference) ++ postPremiseReferences,
        provingContext.depth),
      reference,
      isRearrangement = false)
      ).headOption
  }

  private def matchElidablePremise(
    premise: Statement,
    premiseSubstitutionsSoFar: Substitutions
  ): Iterator[(Reference, Substitutions)] = {
    for {
      inference <- provingContext.availableInferences.iterator
      // Match the premises first, since we don't know what the conclusion should look like
      (premiseReferences, inferenceSubstitutionsAfterPremises) <- matchPremisesToProvenStatements(
        inference.premises,
        provingContext.defaultSubstitutions)
      // Work out the substitutions by condensing the conclusion with the premise
      (premiseSubstitutions, inferenceSubstitutions, _) <- premise.condense(
        inference.conclusion,
        premiseSubstitutionsSoFar,
        inferenceSubstitutionsAfterPremises,
        Nil
      ).iterator
      provenConclusion <- inference.conclusion.applySubstitutions(inferenceSubstitutions).iterator
      finalSubstitutions <- premise.calculateSubstitutions(provenConclusion, premiseSubstitutions, Nil).iterator
    } yield Reference.Elided(
      InferenceApplication.Direct(inference, inferenceSubstitutions, premiseReferences, provingContext.depth)
    ) -> finalSubstitutions
  }

  private def splitPremisesAtElidable(premises: Seq[Premise]): Option[(Seq[Premise], Statement, Seq[Premise])] = {
    val (prePremises, elidableAndPostPremises) = premises.span {
      case premise if premise.isElidable =>
        false
      case _ =>
        true
    }
    elidableAndPostPremises match {
      case Premise(premiseStatement, _) +: postPremises =>
        Some((prePremises, premiseStatement, postPremises))
      case _ =>
        None
    }
  }

  private def matchPremisesToProvenStatements(
    premises: Seq[Premise],
    substitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)] = Nil
  ): Iterator[(Seq[Reference], Substitutions)] = {
    val initial = Iterator((Seq.empty[Reference], substitutions))
    premises.foldLeft(initial) { case (acc, premise) =>
      acc.flatMap { case (referencesSoFar, substitutionsSoFar) =>
        matchPremiseToProvenStatements(premise, substitutionsSoFar, applicativeHints)
          .map { case (premiseReference, newSubstitutions) =>
            (referencesSoFar :+ premiseReference, newSubstitutions)
          }
      }
    }
  }

  private def matchPremiseToProvenStatements(
    premise: Premise,
    substitutionsSoFar: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)]
  ): Iterator[(Reference, Substitutions)] = {
    referencedStatements.iterator.flatMap(matchPremiseToProvenStatement(premise.statement, _, substitutionsSoFar, applicativeHints)) ++
      matchPremiseToRearrangedProvenStatements(premise.statement, substitutionsSoFar, applicativeHints)
  }

  private def matchPremiseToProvenStatement(
    premiseStatement: Statement,
    provenStatement: ReferencedStatement,
    substitutionsSoFar: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)]
  ): Iterator[(Reference, Substitutions)] = {
    premiseStatement.calculateSubstitutions(provenStatement.statement, substitutionsSoFar, applicativeHints)
      .toIterator
      .map { newSubstitutions =>
        (provenStatement.reference, newSubstitutions)
      }
  }

  def matchPremiseToRearrangedProvenStatements(
    premiseStatement: Statement,
    substitutionsSoFar: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)]
  ): Iterator[(Reference, Substitutions)] = {
    for {
      inference <- provingContext.availableInferences.iterator
      if inference.rearrangementType == RearrangementType.Expansion
      (partialPremiseSubstitutions, inferenceConclusionSubstitutions, newApplicativeHints) <- premiseStatement.condense(
        inference.conclusion,
        substitutionsSoFar,
        provingContext.defaultSubstitutions,
        applicativeHints
      ).iterator
      (premiseReferences, inferenceSubstitutions) <- matchPremisesToProvenStatements(
        inference.premises,
        inferenceConclusionSubstitutions,
        newApplicativeHints)
      conclusion <- inference.conclusion.applySubstitutions(inferenceSubstitutions).toSeq
      premiseSubstitutions <- premiseStatement.calculateSubstitutions(conclusion, partialPremiseSubstitutions, Nil)
    } yield Reference.Expansion(
      InferenceApplication.Direct(inference, inferenceSubstitutions, premiseReferences, provingContext.depth)
    ) -> premiseSubstitutions
  }
}

object InferenceProver {
  def apply(
    inference: Inference,
    provenStatements: Seq[ProvenStatement],
    simplifications: Seq[ReferencedStatement])(
   implicit provingContext: ProvingContext
  ): InferenceProver = {
    val baseStatements = provenStatements.map(_.toReferencedStatement)
    val provenStatementsToUse = if (inference.allowsRearrangement)
      baseStatements ++ simplifications
    else
      baseStatements
    InferenceProver(inference, provenStatementsToUse)
  }
}
