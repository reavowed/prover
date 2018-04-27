package net.prover.model.proof

import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.expressions._

import scala.util.Try

case class ProofFinder(statementToProve: Statement)(implicit context: ProvingContext)
{
  import context._

  val simplifications = for {
    provenStatement <- provenStatements
    reference <- provenStatement.reference.asOptionalInstanceOf[Reference.ToSingleLine].toSeq
    simplification <- getAllSimplifications(provenStatement.statement, reference)
  } yield simplification

  lazy val transformations: Seq[Transformation] = scopingStatement.toSeq.flatMap { statementDefinition =>
    for {
      variableName <- statementDefinition.boundVariableNames.single
    } yield {
      Transformation(statementDefinition, variableName)
    }
  }

  def findProof(): Option[InferenceApplication] = {
    Proof.logger.info(s"Proving statement $statementToProve")
    availableInferences.iterator.findFirst(findDirectProof(_, allowRearrangement = false)) orElse
      findProofByRearranging() orElse
      availableInferences.iterator.findFirst(findDirectProof(_, allowRearrangement = true)) orElse
      availableInferences.iterator.findFirst(findProofUsingTransform)
  }

  private def findDirectProof(inference: Inference, allowRearrangement: Boolean): Option[InferenceApplication] = {
    getProofFinderForInference(inference, allowRearrangement).findDirectProof(statementToProve)
  }

  private def findProofUsingTransform(inference: Inference): Option[InferenceApplication] = {
    getProofFinderForInference(inference).findProofUsingTransform(statementToProve)
  }

  def findProofByRearranging(): Option[InferenceApplication] = {
    Proof.logger.info("  by rearranging")
    ProofFinder.findAssertionByExpanding(
      statementToProve,
      provenStatements.map(_.toReferencedStatement) ++ simplifications)
  }

  def findProofByEliding(elidedStatement: Statement): Option[InferenceApplication] = {
    for {
      elidedReference <- ProofFinder(elidedStatement).findProof().map(Reference.Elided.apply)
      result <- availableInferences.iterator.findFirst(findProofByEliding(_, elidedStatement, elidedReference))
    } yield result
  }

  private def findProofByEliding(inference: Inference, elidedStatement: Statement, elidedReference: Reference.Elided): Option[InferenceApplication] = {
    getProofFinderForInference(inference).findProofByEliding(statementToProve, elidedStatement, elidedReference)
  }

  private def getAllSimplifications(
    statement: Statement,
    reference: Reference.ToSingleLine
  ): Seq[ReferencedStatement] = {
    def helper(
      next: Seq[(Statement, Reference.ToSingleLine)],
      acc: Seq[(Statement, Reference.ToSingleLine)]
    ): Seq[(Statement, Reference.ToSingleLine)] = {
      if (next.isEmpty)
        acc
      else {
        val newSimplifications = next.flatMap((getNextLevelSimplifications _).tupled)
        helper(newSimplifications, acc ++ newSimplifications)
      }
    }
    helper(Seq((statement, reference)), Nil).map((ReferencedStatement.apply _).tupled)
  }

  private def getNextLevelSimplifications(
    statement: Statement,
    reference: Reference.ToSingleLine
  ): Seq[(Statement, Reference.ToSingleLine)] = {
    for {
      inference <- availableInferences
      if inference.rearrangementType == RearrangementType.Simplification
      premise <- inference.premises.single.toSeq
      simplificationPath <- premise.statement.findComponentPath(inference.conclusion).toSeq
      substitutions <- premise.statement.calculateSubstitutions(statement, defaultSubstitutions, Nil, Nil)
      conclusion <- inference.conclusion.applySubstitutions(substitutions).toSeq
    } yield conclusion -> Reference.Simplification(
      inference,
      substitutions,
      reference,
      simplificationPath,
      depth)
  }

  def getProofFinderForInference(inference: Inference, allowRearrangement: Boolean = true): InferenceProofFinder = {
    InferenceProofFinder(inference, provenStatements, simplifications, allowRearrangement)
  }
}

object ProofFinder {
  def findAssertionWithPossibleExpansions(
    assertion: Statement,
    referencedStatements: Seq[ReferencedStatement])(
    implicit context: ProvingContext
  ): Option[Reference] = {
    findAssertionDirectly(assertion, referencedStatements) orElse
      findAssertionByExpanding(assertion, referencedStatements).map(Reference.Expansion)
  }

  def findAssertionByExpanding(
    assertion: Statement,
    referencedStatements: Seq[ReferencedStatement])(
    implicit provingContext: ProvingContext
  ): Option[InferenceApplication] = {
    findAssertionByExpandingWithoutTransformation(assertion, referencedStatements) orElse
      findAssertionByExpandingWithTransformation(assertion, referencedStatements)
  }

  def findAssertionByExpandingWithoutTransformation(
    assertion: Statement,
    referencedStatements: Seq[ReferencedStatement])(
    implicit provingContext: ProvingContext
  ): Option[InferenceApplication] = {
    (for {
      inference <- provingContext.availableInferences.iterator
      if inference.rearrangementType == RearrangementType.Expansion
      substitutions <- inference.conclusion.calculateSubstitutions(assertion, provingContext.defaultSubstitutions, Nil, Nil)
      substitutedPremises <- inference.premises.map(_.statement.applySubstitutions(substitutions)).traverseOption.toSeq
      premiseReferences <- substitutedPremises.map(findAssertionWithPossibleExpansions(_, referencedStatements)).traverseOption.toSeq
      if inference.conclusion.applySubstitutions(substitutions).contains(assertion)
    } yield InferenceApplication.Direct(inference, substitutions, premiseReferences, isRearrangement = true, provingContext.depth)).headOption
  }

  def findAssertionByExpandingWithTransformation(
    assertion: Statement,
    referencedStatements: Seq[ReferencedStatement])(
    implicit provingContext: ProvingContext
  ): Option[InferenceApplication] = {
    if (provingContext.allowTransformations) {
      (for {
        transformation <- provingContext.scopingStatement.flatMap(Transformation.apply).iterator
        inference <- provingContext.availableInferences
        if inference.rearrangementType == RearrangementType.Expansion
        (transformedPremises, transformedConclusion, stepsToProve) <- transformation.applyFully(inference).iterator
        substitutions <- transformedConclusion.calculateSubstitutions(assertion, provingContext.defaultSubstitutions, Nil, Nil)
        substitutedPremises <- transformedPremises.map(_.statement.applySubstitutions(substitutions)).traverseOption.toSeq
        premiseReferences <- substitutedPremises.map(findAssertionWithPossibleExpansions(_, referencedStatements)).traverseOption.toSeq
        if transformedConclusion.applySubstitutions(substitutions).contains(assertion)
        transformationProofAttempt = Try(ProofOutline(stepsToProve)
          .fillIn(provingContext.resetWithPremises(transformedPremises).copy(allowTransformations = false)))
        transformationProof <- transformationProofAttempt.toOption
      } yield InferenceApplication.Transformed(
        inference,
        substitutions,
        premiseReferences,
        transformation.statementDefinition,
        transformedPremises,
        transformedConclusion,
        transformationProof.steps,
        isRearrangement = true,
        provingContext.depth)
      ).headOption
    } else None
  }

  def findAssertionDirectly(assertion: Statement, referencedStatements: Seq[ReferencedStatement]): Option[Reference] = {
    referencedStatements.find(_.statement == assertion).map(_.reference)
  }
}
