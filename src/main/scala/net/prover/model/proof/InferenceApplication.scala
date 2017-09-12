package net.prover.model.proof

import net.prover.model._
import net.prover.model.components.Statement
import net.prover.model.entries.StatementDefinition

sealed trait InferenceApplication {
  def inference: Inference
  def referencedInferenceIds: Set[String]
  def directReferences: Set[Reference.ToFact]
  def cached: CachedInferenceApplication
}

object InferenceApplication {
  case class Direct(
      inference: Inference,
      substitutions: Substitutions,
      references: Seq[Reference])
    extends InferenceApplication
  {
    def referencedInferenceIds = references.flatMap(_.referencedInferenceIds).toSet + inference.id
    def directReferences = references.flatMap(_.factReferences).toSet
    def cached = CachedInferenceApplication.Direct(inference.id, inference.specifySubstitutions(substitutions).get, references.map(_.cached))
  }

  case class Transformed(
      inference: Inference,
      substitutions: Substitutions,
      references: Seq[Reference],
      transformation: StatementDefinition,
      transformedPremises: Seq[Premise],
      transformedConclusion: Statement,
      transformationProof: Seq[Step])
    extends InferenceApplication
  {
    def referencedInferenceIds = references.flatMap(_.referencedInferenceIds).toSet ++
      transformationProof.flatMap(_.referencedInferenceIds) +
      inference.id
    def directReferences = references.flatMap(_.factReferences).toSet
    def cached = CachedInferenceApplication.Transformed(
      inference.id,
      Inference.Transformed(inference, transformedPremises, transformedConclusion).specifySubstitutions(substitutions).get,
      references.map(_.cached),
      transformation,
      transformedPremises,
      transformationProof.map(_.cached))
  }
}
