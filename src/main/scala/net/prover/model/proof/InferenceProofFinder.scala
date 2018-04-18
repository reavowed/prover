package net.prover.model.proof

import net.prover.model.Inference.RearrangementType
import net.prover.model.expressions.{ArgumentList, Statement}
import net.prover.model._

import scala.util.Try

case class InferenceProofFinder(
  inference: Inference,
  referencedStatements: Seq[ReferencedStatement],
  allowRearrangment: Boolean)(
  implicit provingContext: ProvingContext)
{
  def findDirectProof(
    assertionToProve: Statement,
    reference: Reference.Direct
  ): Option[Step.Assertion] = {
    (for {
      conclusionSubstitutions <- inference.conclusion
        .calculateSubstitutions(assertionToProve, provingContext.defaultSubstitutions, Nil, Nil)
        .iterator
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

  def findProofUsingTransform(
    assertionToProve: Statement,
    reference: Reference.Direct
  ): Option[Step.Assertion] = {
    if (provingContext.allowTransformations) {
      for {
        transformation <- provingContext.scopingStatement.flatMap(Transformation.apply)
        transformationSubstitutions <- transformation.getSubstitutions(inference)
        result <- findProofUsingFullTransform(assertionToProve, reference, transformation, transformationSubstitutions) orElse
          findProofUsingPartialTransform(assertionToProve, reference, transformation, transformationSubstitutions)
      } yield result
    } else None
  }

  def findProofUsingFullTransform(
    assertionToProve: Statement,
    reference: Reference.Direct,
    transformation: Transformation,
    transformationSubstitutions: Substitutions
  ): Option[Step.Assertion] = {
    (for {
      (transformedPremises, transformedConclusion, stepsToProve) <- transformation.applyFully(inference, transformationSubstitutions).iterator
      conclusionSubstitutions <- transformedConclusion
        .calculateSubstitutions(assertionToProve, provingContext.defaultSubstitutions, Nil, Nil)
        .iterator
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

  def findProofUsingPartialTransform(
    assertionToProve: Statement,
    reference: Reference.Direct,
    transformation: Transformation,
    transformationSubstitutions: Substitutions
  ): Option[Step.Assertion] = {
    (for {
      (transformedPremisesAndSteps, transformedConclusion, conclusionStepToProve) <- transformation
        .applyPartially(inference, transformationSubstitutions)
        .iterator
      conclusionSubstitutions <- transformedConclusion
        .calculateSubstitutions(assertionToProve, provingContext.defaultSubstitutions, Nil, Nil)
        .iterator
      (transformedPremises, premiseReferences, premiseStepsToProve, premiseSubstitutions) <- transformedPremisesAndSteps
        .foldLeft(Iterator((Seq.empty[Premise], Seq.empty[Reference], Seq.empty[StepOutline], conclusionSubstitutions))) { case (acc, premisesAndSteps) =>
            for {
              (premisesSoFar, referencesSoFar, stepOutlinesSoFar, substitutionsSoFar) <- acc
              (premise, stepOption) <- premisesAndSteps
              (reference, substitutions) <- matchPremiseToProvenStatements(premise, substitutionsSoFar, Nil, Nil)
            } yield (premisesSoFar :+ premise, referencesSoFar :+ reference, stepOutlinesSoFar ++ stepOption, substitutions)
        }
      stepsToProve = premiseStepsToProve :+ conclusionStepToProve
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

  def findProofByEliding(
    assertionToProve: Statement,
    reference: Reference.Direct
  ): Option[Step.Assertion] = {
    (for {
      (prePremises, elidablePremise, postPremises) <- inference.premises.splitAtAll(_.isElidable).iterator
      substitutionsAfterConclusion <- inference.conclusion.calculateSubstitutions(assertionToProve, provingContext.defaultSubstitutions, Nil, Nil)
      (prePremiseReferences, substitutionsAfterPrePremises) <- matchPremisesToProvenStatements(
        prePremises,
        substitutionsAfterConclusion)
      (postPremiseReferences, substitutionsAfterPostPremises) <- matchPremisesToProvenStatements(
        postPremises,
        substitutionsAfterPrePremises)
      (elidedPremiseReference, substitutionsAfterElidedPremise) <- matchElidablePremise(
        elidablePremise.statement,
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
      (premiseSubstitutions, inferenceSubstitutions, _, _) <- premise.condense(
        inference.conclusion,
        premiseSubstitutionsSoFar,
        inferenceSubstitutionsAfterPremises,
        Nil,
        Nil
      ).iterator
      provenConclusion <- inference.conclusion.applySubstitutions(inferenceSubstitutions).iterator
      finalSubstitutions <- premise.calculateSubstitutions(provenConclusion, premiseSubstitutions, Nil, Nil).iterator
    } yield Reference.Elided(
      InferenceApplication.Direct(inference, inferenceSubstitutions, premiseReferences, provingContext.depth)
    ) -> finalSubstitutions
  }

  private def matchPremisesToProvenStatements(
    premises: Seq[Premise],
    substitutions: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)] = Nil,
    structuralHints: Seq[Substitutions] = Nil
  ): Iterator[(Seq[Reference], Substitutions)] = {
    val initial = Iterator((Seq.empty[Reference], substitutions))
    premises.foldLeft(initial) { case (acc, premise) =>
      acc.flatMap { case (referencesSoFar, substitutionsSoFar) =>
        matchPremiseToProvenStatements(premise, substitutionsSoFar, applicativeHints, structuralHints)
          .map { case (premiseReference, newSubstitutions) =>
            (referencesSoFar :+ premiseReference, newSubstitutions)
          }
      }
    }
  }

  private def matchPremiseToProvenStatements(
    premise: Premise,
    substitutionsSoFar: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)],
    structuralHints: Seq[Substitutions]
  ): Iterator[(Reference, Substitutions)] = {
    val directMatches = referencedStatements.iterator
      .flatMap {
        matchPremiseToProvenStatement(premise.statement, _, substitutionsSoFar, applicativeHints, structuralHints)
      }
    if (allowRearrangment && inference.allowsRearrangement)
      directMatches ++
        matchPremiseToRearrangedProvenStatements(premise.statement, substitutionsSoFar, applicativeHints, structuralHints) ++
        matchPremiseToTransformedRearrangedProvenStatements(premise.statement, substitutionsSoFar, applicativeHints, structuralHints)
    else
      directMatches
  }

  private def matchPremiseToProvenStatement(
    premiseStatement: Statement,
    provenStatement: ReferencedStatement,
    substitutionsSoFar: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)],
    structuralHints: Seq[Substitutions]
  ): Iterator[(Reference, Substitutions)] = {
    premiseStatement.calculateSubstitutions(provenStatement.statement, substitutionsSoFar, applicativeHints, structuralHints)
      .toIterator
      .map { newSubstitutions =>
        (provenStatement.reference, newSubstitutions)
      }
  }

  def matchPremiseToRearrangedProvenStatements(
    premiseStatement: Statement,
    substitutionsSoFar: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)],
    structuralHints: Seq[Substitutions]
  ): Iterator[(Reference, Substitutions)] = {
    for {
      inference <- provingContext.availableInferences.iterator
      if inference.rearrangementType == RearrangementType.Expansion
      (partialPremiseSubstitutions, inferenceConclusionSubstitutions, newApplicativeHints, newStructuralHints) <- premiseStatement.condense(
        inference.conclusion,
        substitutionsSoFar,
        provingContext.defaultSubstitutions,
        applicativeHints,
        structuralHints
      ).iterator
      (premiseReferences, inferenceSubstitutions) <- matchPremisesToProvenStatements(
        inference.premises,
        inferenceConclusionSubstitutions,
        newApplicativeHints,
        newStructuralHints)
      conclusion <- inference.conclusion.applySubstitutions(inferenceSubstitutions).toSeq
      premiseSubstitutions <- premiseStatement.calculateSubstitutions(conclusion, partialPremiseSubstitutions, Nil, Nil)
    } yield Reference.Expansion(
      InferenceApplication.Direct(inference, inferenceSubstitutions, premiseReferences, provingContext.depth)
    ) -> premiseSubstitutions
  }

  def matchPremiseToTransformedRearrangedProvenStatements(
    premiseStatement: Statement,
    substitutionsSoFar: Substitutions,
    applicativeHints: Seq[(Substitutions, ArgumentList)],
    structuralHints: Seq[Substitutions]
  ): Iterator[(Reference, Substitutions)] = {
    if (provingContext.allowTransformations) {
      for {
        transformation <- provingContext.scopingStatement.flatMap(Transformation.apply).iterator
        inference <- provingContext.availableInferences
        if inference.rearrangementType == RearrangementType.Expansion
        (transformedPremises, transformedConclusion, stepsToProve) <- transformation.applyFully(inference).iterator
        (partialPremiseSubstitutions, inferenceConclusionSubstitutions, newApplicativeHints, newStructuralHints) <- premiseStatement.condense(
          transformedConclusion,
          substitutionsSoFar,
          provingContext.defaultSubstitutions,
          applicativeHints,
          structuralHints)
        (premiseReferences, inferenceSubstitutions) <- matchPremisesToProvenStatements(
          transformedPremises,
          inferenceConclusionSubstitutions,
          newApplicativeHints,
          newStructuralHints)
        conclusion <- transformedConclusion.applySubstitutions(inferenceSubstitutions).toSeq
        premiseSubstitutions <- premiseStatement.calculateSubstitutions(conclusion, partialPremiseSubstitutions, Nil, Nil)
        transformationProofAttempt = Try(ProofOutline(stepsToProve)
          .fillIn(provingContext.resetWithPremises(transformedPremises).copy(allowTransformations = false)))
        transformationProof <- transformationProofAttempt.toOption
      } yield Reference.Expansion(
        InferenceApplication.Transformed(
          inference,
          inferenceSubstitutions,
          premiseReferences,
          transformation.statementDefinition,
          transformedPremises,
          transformedConclusion,
          transformationProof.steps,
          provingContext.depth)
      ) -> premiseSubstitutions
    } else Iterator.empty
  }
}

object InferenceProofFinder {
  def apply(
    inference: Inference,
    provenStatements: Seq[ProvenStatement],
    simplifications: Seq[ReferencedStatement],
    allowRearrangment: Boolean)(
   implicit provingContext: ProvingContext
  ): InferenceProofFinder = {
    val baseStatements = provenStatements.map(_.toReferencedStatement)
    val provenStatementsToUse = if (allowRearrangment && inference.allowsRearrangement)
      baseStatements ++ simplifications
    else
      baseStatements
    InferenceProofFinder(inference, provenStatementsToUse, allowRearrangment)
  }
}
