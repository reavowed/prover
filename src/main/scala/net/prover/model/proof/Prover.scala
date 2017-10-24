package net.prover.model.proof

import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.expressions._

import scala.util.Try

case class Prover(
  assertionToProve: Statement,
  reference: Reference.Direct,
  context: ProvingContext)
{
  import context._

  val applicableHints = assertionHints.filter(_.conclusion == assertionToProve)
  val provenStatementsWithSimplifications = {
    val simplifications = for {
      provenStatement <- provenStatements
      reference <- provenStatement.reference.asOptionalInstanceOf[Reference.ToSingleLine].toSeq
      simplification <- getAllSimplifications(provenStatement.statement, reference)
    } yield simplification
    provenStatements.map(f => f.statement -> f.reference) ++ simplifications
  }

  lazy val transformations: Seq[Transformation] = scopingStatement.toSeq.flatMap { statementDefinition =>
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
        matchPremisesToProvenStatements(inference.premises, substitutions, inference.allowsRearrangement)
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
      if allowTransformations
      (transformedPremises, transformedConclusion, stepsToProve) <- transformation.applyToInference(inference.premises, inference.conclusion)
      conclusionSubstitutions <- transformedConclusion.calculateSubstitutions(assertionToProve, Substitutions.emptyWithDepth(depth))
      (premiseReferences, premiseSubstitutions) <- matchPremisesToProvenStatements(
        transformedPremises,
        conclusionSubstitutions,
        inference.allowsRearrangement)
      transformationProofAttempt = Try(ProofOutline(stepsToProve).fillIn(ProvingContext.getInitial(
        transformedPremises,
        assertionHints,
        availableInferences,
        deductionStatement.toSeq ++ scopingStatement.toSeq
      ).copy(allowTransformations = false)))
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
      (prePremiseReferences, substitutionsAfterPrePremises) <- matchPremisesToProvenStatements(
        prePremises,
        substitutionsAfterConclusion,
        inference.allowsRearrangement)
      (postPremiseReferences, substitutionsAfterPostPremises) <- matchPremisesToProvenStatements(
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
      (premiseReferences, inferenceSubstitutionsAfterPremises) <- matchPremisesToProvenStatements(
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
      case Premise(premiseStatement, _) +: postPremises =>
        Some((prePremises, premiseStatement, postPremises))
      case _ =>
        None
    }
  }

  def proveAssertionByRearranging(): Option[Step.Assertion] = {
    val expansions = availableInferences
      .filter(_.rearrangementType == RearrangementType.Expansion)
    def findAssertionByExpanding(assertion: Statement): Option[InferenceApplication] = {
      expansions.iterator
        .findFirst { inference => (
          for {
            substitutions <- inference.conclusion.calculateSubstitutions(assertion, Substitutions.emptyWithDepth(depth))
            substitutedPremises <- inference.premises.map(_.statement.applySubstitutions(substitutions)).traverseOption.toSeq
            premiseReferences <- substitutedPremises.map(getAssertionByRearranging).traverseOption.toSeq
            if inference.conclusion.applySubstitutions(substitutions).contains(assertion)
          } yield InferenceApplication.Direct(inference, substitutions, premiseReferences, depth)
        ).headOption}
    }
    def getAssertionByRearranging(assertion: Statement): Option[Reference] = {
      findAssertionInProvenStatements(assertion) orElse findAssertionByExpanding(assertion).map(Reference.Expansion)
    }
    findAssertionByExpanding(assertionToProve).map { inferenceApplication =>
      Step.Assertion(assertionToProve, inferenceApplication, reference, isRearrangement = true)
    }
  }

  def findAssertionInProvenStatements(assertion: Statement): Option[Reference] = {
    provenStatementsWithSimplifications.find(_._1 == assertion).map(_._2)
  }

  private def matchPremisesToProvenStatements(
    premises: Seq[Premise],
    substitutions: Substitutions,
    rearrangementAllowed: Boolean
  ): Iterator[(Seq[Reference], Substitutions)] = {
    val initial = Iterator((Seq.empty[Reference], substitutions))
    premises.foldLeft(initial) { case (acc, premise) =>
      acc.flatMap { case (referencesSoFar, substitutionsSoFar) =>
        matchPremiseToProvenStatements(premise, substitutionsSoFar, rearrangementAllowed).map { case (premiseReference, newSubstitutions) =>
          (referencesSoFar :+ premiseReference, newSubstitutions)
        }
      }
    }
  }

  private def matchPremiseToProvenStatements(
    premise: Premise,
    substitutionsSoFar: Substitutions,
    allowRearrangement: Boolean
  ): Iterator[(Reference, Substitutions)] = {
    val provenStatementsToUse = if (allowRearrangement)
      provenStatementsWithSimplifications
    else
      provenStatements.map(f => f.statement -> f.reference)
    provenStatementsToUse.iterator.flatMap { case (statement, statementReference) =>
      matchPremiseToProvenStatement(premise.statement, statement, statementReference, substitutionsSoFar)
    }
  }

  private def matchPremiseToProvenStatement(
    premiseStatement: Statement,
    provenStatement: Statement,
    statementReference: Reference,
    substitutionsSoFar: Substitutions
  ): Iterator[(Reference, Substitutions)] = {
    premiseStatement.calculateSubstitutions(provenStatement, substitutionsSoFar)
      .toIterator
      .map { newSubstitutions =>
        (statementReference, newSubstitutions)
      }
  }

  private def getAllSimplifications(
    statement: Statement,
    reference: Reference.ToSingleLine
  ): Seq[(Statement, Reference)] = {
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
    helper(Seq((statement, reference)), Nil)
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
      substitutions <- premise.statement.calculateSubstitutions(statement, Substitutions.emptyWithDepth(depth))
      conclusion <- inference.conclusion.applySubstitutions(substitutions).toSeq
    } yield conclusion -> Reference.Simplification(inference, substitutions, reference, simplificationPath, depth)
  }
}
