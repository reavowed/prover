package net.prover.model.proof

import net.prover.model.Inference.RearrangementType
import net.prover.model._
import net.prover.model.expressions._

case class ProofFinder(
  assertionToProve: Statement,
  reference: Reference.Direct)(
  implicit context: ProvingContext)
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

  def proveAssertion(): Option[Step.Assertion] = {
    availableInferences.iterator.findFirst(proveDirectly) orElse
      availableInferences.iterator.findFirst(proveUsingTransformedInference) orElse
      availableInferences.iterator.findFirst(proveUsingElidedInference) orElse
      proveAssertionByRearranging()
  }

  private def proveDirectly(inference: Inference): Option[Step.Assertion] = {
    proverForInference(inference).proveDirectly(assertionToProve, reference)
  }

  private def proveUsingTransformedInference(inference: Inference): Option[Step.Assertion] = {
    proverForInference(inference).proveWithTransformation(assertionToProve, reference)
  }

  private def proveUsingElidedInference(inference: Inference): Option[Step.Assertion] = {
    proverForInference(inference).proveByEliding(assertionToProve, reference)
  }

  def proveAssertionByRearranging(): Option[Step.Assertion] = {
    ProofFinder.findAssertionByExpanding(
      assertionToProve,
      provenStatements.map(_.toReferencedStatement) ++ simplifications
    ).map { inferenceApplication =>
      Step.Assertion(assertionToProve, inferenceApplication, reference, isRearrangement = true)
    }
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

  def proverForInference(inference: Inference): InferenceProver = {
    InferenceProver(inference, provenStatements, simplifications)
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
    (for {
      inference <- provingContext.availableInferences.iterator
      if inference.rearrangementType == RearrangementType.Expansion
      substitutions <- inference.conclusion.calculateSubstitutions(assertion, provingContext.defaultSubstitutions, Nil, Nil)
      substitutedPremises <- inference.premises.map(_.statement.applySubstitutions(substitutions)).traverseOption.toSeq
      premiseReferences <- substitutedPremises.map(findAssertionWithPossibleExpansions(_, referencedStatements)).traverseOption.toSeq
      if inference.conclusion.applySubstitutions(substitutions).contains(assertion)
    } yield InferenceApplication.Direct(inference, substitutions, premiseReferences, provingContext.depth)).headOption
  }

  def findAssertionDirectly(assertion: Statement, referencedStatements: Seq[ReferencedStatement]): Option[Reference] = {
    referencedStatements.find(_.statement == assertion).map(_.reference)
  }
}
