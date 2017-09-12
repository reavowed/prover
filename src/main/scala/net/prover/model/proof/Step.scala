package net.prover.model.proof

import net.prover.model._
import net.prover.model.components.{Statement, TermVariable}

sealed trait Step {
  def reference: Reference.Direct
  def fact: Option[Fact]
  def referencedFact: Option[ReferencedFact] = fact.map { f => ReferencedFact(f, reference)}
  def referencedInferenceIds: Set[String]
  def referenceMap: ReferenceMap
  def cached: CachedStep
}

object Step {

  sealed trait WithProvenStatement extends Step {
    def statement: Statement
    override def fact = Some(Fact.Direct(statement))
  }

  case class Assertion(
    statement: Statement,
    inferenceApplication: InferenceApplication,
    reference: Reference.Direct,
    isRearrangement: Boolean)
    extends Step.WithProvenStatement
  {
    override def referencedInferenceIds: Set[String] = inferenceApplication.referencedInferenceIds
    override def referenceMap: ReferenceMap = ReferenceMap(reference.value -> inferenceApplication.directReferences)
    override def cached = CachedStep.Assertion(statement, inferenceApplication.cached, reference, isRearrangement)
  }

  case class Assumption(
      assumption: Statement,
      steps: Seq[Step],
      reference: Reference.Direct)
    extends Step
  {
    def assumptionReference = reference.withSuffix("a")
    override val fact = steps.ofType[Step.WithProvenStatement].lastOption.map(lastSubstep => Fact.Deduced(assumption, lastSubstep.statement))
    override def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
    override def referenceMap: ReferenceMap = steps.map(_.referenceMap).foldTogether
    override def cached = CachedStep.Assumption(assumption, steps.map(_.cached), reference)
  }

  case class Naming(
      variable: TermVariable,
      assumptionStep: Step.Assumption,
      assertionStep: Step.Assertion,
      reference: Reference.Direct)
    extends Step.WithProvenStatement
  {
    override def statement: Statement = assertionStep.statement
    override def referencedInferenceIds: Set[String] = assumptionStep.referencedInferenceIds ++ assertionStep.referencedInferenceIds
    override def referenceMap: ReferenceMap = assertionStep.referenceMap
    override def cached = CachedStep.Naming(variable, assumptionStep.cached, assertionStep.cached, reference)
  }

  case class ScopedVariable(boundVariableName: String, substeps: Seq[Step], reference: Reference.Direct) extends Step {
    override def fact: Option[Fact] = {
      substeps.ofType[Step.WithProvenStatement].lastOption
        .map(_.statement)
        .map(Fact.Bound(_)(boundVariableName))
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referenceMap: ReferenceMap = substeps.map(_.referenceMap).foldTogether
    override def cached = CachedStep.ScopedVariable(boundVariableName, substeps.map(_.cached), reference)
  }
}
