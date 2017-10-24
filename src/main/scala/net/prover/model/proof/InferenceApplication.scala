package net.prover.model.proof

import net.prover.model._
import net.prover.model.expressions.Statement
import net.prover.model.entries.StatementDefinition

sealed trait InferenceApplication {
  def inference: Inference
  def referencedInferenceIds: Set[String]
  def references: Seq[Reference]
  def lineReferences: Set[(String, Seq[Int])]
  def cached: CachedInferenceApplication
}

object InferenceApplication {
  case class Direct(
      inference: Inference,
      substitutions: Substitutions,
      references: Seq[Reference],
      depth: Int)
    extends InferenceApplication
  {
    def referencedInferenceIds = references.flatMap(_.referencedInferenceIds).toSet + inference.id
    def lineReferences = references.flatMap(_.lineReferences).toSet
    def cached = CachedInferenceApplication.Direct(
      inference.id,
      inference.specifySubstitutions(substitutions),
      references.map(_.cached),
      depth)
  }

  case class Transformed(
      inference: Inference,
      substitutions: Substitutions,
      references: Seq[Reference],
      transformation: StatementDefinition,
      transformedPremises: Seq[Premise],
      transformedConclusion: Statement,
      transformationProof: Seq[Step],
      depth: Int)
    extends InferenceApplication
  {
    def referencedInferenceIds = references.flatMap(_.referencedInferenceIds).toSet ++
      transformationProof.flatMap(_.referencedInferenceIds) +
      inference.id
    def lineReferences = references.flatMap(_.lineReferences).toSet
    def cached = CachedInferenceApplication.Transformed(
      inference.id,
      Inference.Transformed(inference, transformedPremises, transformedConclusion).specifySubstitutions(substitutions),
      references.map(_.cached),
      transformation,
      transformedPremises,
      transformationProof.map(_.cached),
      depth)
  }
}
