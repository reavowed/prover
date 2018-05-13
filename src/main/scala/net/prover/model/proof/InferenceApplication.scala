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
  def isRearrangement: Boolean
  def conclusion: Statement
}

object InferenceApplication {
  case class Direct(
      inference: Inference,
      substitutions: Substitutions,
      references: Seq[Reference],
      isRearrangement: Boolean)
    extends InferenceApplication
  {
    override def referencedInferenceIds = references.flatMap(_.referencedInferenceIds).toSet + inference.id
    override def lineReferences = references.flatMap(_.lineReferences).toSet
    override def cached = CachedInferenceApplication.Direct(
      inference.id,
      inference.specifySubstitutions(substitutions),
      references.map(_.cached),
      isRearrangement)
    override def conclusion = inference.conclusion.applySubstitutions(substitutions, 0, 0).get
  }

  case class Transformed(
      inference: Inference,
      substitutions: Substitutions,
      references: Seq[Reference],
      transformation: StatementDefinition,
      transformedPremises: Seq[Premise],
      transformedConclusion: Statement,
      transformationProof: Seq[Step],
      isRearrangement: Boolean)
    extends InferenceApplication
  {
    override def referencedInferenceIds = references.flatMap(_.referencedInferenceIds).toSet ++
      transformationProof.flatMap(_.referencedInferenceIds) +
      inference.id
    override def lineReferences = references.flatMap(_.lineReferences).toSet
    override def cached = CachedInferenceApplication.Transformed(
      inference.id,
      Inference.Transformed(inference, transformedPremises, transformedConclusion).specifySubstitutions(substitutions),
      references.map(_.cached),
      transformation,
      transformedPremises,
      transformationProof.map(_.cached),
      isRearrangement)
    override def conclusion = transformedConclusion.applySubstitutions(substitutions, 0, 0).get
  }
}
