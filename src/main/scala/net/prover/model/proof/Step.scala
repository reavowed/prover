package net.prover.model.proof

import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions.{DefinedStatement, Statement}

sealed trait Step {
  def reference: Reference.Direct
  def facts: Seq[ProvenFact]
  def referencedInferenceIds: Set[String]
  def referenceMap: ReferenceMap
  def cached: CachedStep
  def length: Int
  def intermediateReferences: Seq[String]
}

object Step {
  case class Assertion(
    assertion: Statement,
    inferenceApplication: InferenceApplication,
    reference: Reference.Direct,
    isRearrangement: Boolean)
    extends Step
  {
    override def facts = Seq(ProvenFact(assertion, reference))
    override def referencedInferenceIds = inferenceApplication.referencedInferenceIds
    override def referenceMap = ReferenceMap(reference.value -> inferenceApplication.lineReferences)
    override def cached = CachedStep.Assertion(assertion, inferenceApplication.cached, reference, isRearrangement)
    override def length = 1
    override def intermediateReferences = Seq(reference.value)
  }

  case class Assumption(
      assumption: Statement,
      steps: Seq[Step],
      deductionStatement: StatementDefinition,
      reference: Reference.Direct)
    extends Step
  {
    override def facts = steps.flatMap(_.facts).map { fact =>
      ProvenFact(
        DefinedStatement(Seq(assumption, fact.statement), deductionStatement, assumption.depth)(Nil),
        fact.reference.add(reference))
    }
    override def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
    override def referenceMap: ReferenceMap = steps.map(_.referenceMap).foldTogether
    override def cached = CachedStep.Assumption(assumption, steps.map(_.cached), reference)
    override def length = steps.map(_.length).sum
    override def intermediateReferences = steps.dropRight(1).flatMap(_.intermediateReferences) :+ reference.value
  }

  case class Naming(
      variableName: String,
      assumptionStep: Step.Assumption,
      assertionStep: Step.Assertion,
      reference: Reference.Direct)
    extends Step
  {
    override def facts = Seq(ProvenFact(assertionStep.assertion, reference))
    override def referencedInferenceIds = assumptionStep.referencedInferenceIds ++ assertionStep.referencedInferenceIds
    override def referenceMap = assumptionStep.referenceMap ++ assertionStep.referenceMap
    override def cached = CachedStep.Naming(variableName, assumptionStep.cached, assertionStep.cached, reference)
    override def length = assumptionStep.length + 1
    override def intermediateReferences = assumptionStep.steps.dropRight(1).flatMap(_.intermediateReferences) :+ reference.value
  }

  case class ScopedVariable(
      variableName: String,
      substeps: Seq[Step],
      scopingStatement: StatementDefinition,
      reference: Reference.Direct)
    extends Step
  {
    override def facts = {
      substeps.flatMap(_.facts)
        .map { fact =>
          ProvenFact(
            DefinedStatement(Seq(fact.statement), scopingStatement, fact.statement.depth - 1)(Seq(variableName)),
            fact.reference.add(reference))
        }
    }
    override def referencedInferenceIds: Set[String] = substeps.flatMap(_.referencedInferenceIds).toSet
    override def referenceMap: ReferenceMap = substeps.map(_.referenceMap).foldTogether
    override def cached = CachedStep.ScopedVariable(variableName, substeps.map(_.cached), reference)
    override def length = substeps.map(_.length).sum
    override def intermediateReferences = substeps.dropRight(1).flatMap(_.intermediateReferences) :+ reference.value
  }
}
