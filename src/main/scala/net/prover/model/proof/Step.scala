package net.prover.model.proof

import net.prover.model.expressions.{Statement, TermVariable}

sealed trait Step {
  def reference: Reference.Direct
  def fact: Option[Fact]
  def referencedFact: Option[ReferencedFact] = fact.map { f => ReferencedFact(f, reference)}
  def referencedInferenceIds: Set[String]
  def referenceMap: ReferenceMap
  def cached: CachedStep
}

object Step {

  sealed trait WithAssertion extends Step {
    def assertion: Statement
    override def fact = Some(Fact.Direct(assertion))
  }

  case class Assertion(
    assertion: Statement,
    inferenceApplication: InferenceApplication,
    reference: Reference.Direct,
    isRearrangement: Boolean)
    extends Step.WithAssertion
  {
    override def referencedInferenceIds = inferenceApplication.referencedInferenceIds
    override def referenceMap = ReferenceMap(reference.value -> inferenceApplication.directReferences)
    override def cached = CachedStep.Assertion(assertion, inferenceApplication.cached, reference, isRearrangement)
  }

  case class Assumption(
      assumption: Statement,
      steps: Seq[Step],
      reference: Reference.Direct)
    extends Step
  {
    def assumptionReference = reference.withSuffix("a")
    override val fact = steps.lastOption.flatMap(_.fact).map(Fact.Deduced(assumption, _))
    override def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
    override def referenceMap: ReferenceMap = steps.map(_.referenceMap).foldTogether
    override def cached = CachedStep.Assumption(assumption, steps.map(_.cached), reference)
  }

  case class Naming(
      variableName: String,
      assumptionStep: Step.Assumption,
      assertionStep: Step.Assertion,
      reference: Reference.Direct)
    extends Step.WithAssertion
  {
    override def assertion = assertionStep.assertion
    override def referencedInferenceIds = assumptionStep.referencedInferenceIds ++ assertionStep.referencedInferenceIds
    override def referenceMap = assumptionStep.referenceMap ++ assertionStep.referenceMap
    override def cached = CachedStep.Naming(variableName, assumptionStep.cached, assertionStep.cached, reference)
  }

  case class ScopedVariable(variableName: String, substeps: Seq[Step], reference: Reference.Direct) extends Step {
    override def fact: Option[Fact] = {
      substeps.lastOption.flatMap(_.fact)
        .map(Fact.ScopedVariable(_)(variableName))
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referenceMap: ReferenceMap = substeps.map(_.referenceMap).foldTogether
    override def cached = CachedStep.ScopedVariable(variableName, substeps.map(_.cached), reference)
  }
}
