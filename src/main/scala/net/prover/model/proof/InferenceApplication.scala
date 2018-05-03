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
      isRearrangement: Boolean,
      depth: Int)
    extends InferenceApplication
  {
    override def referencedInferenceIds = references.flatMap(_.referencedInferenceIds).toSet + inference.id
    override def lineReferences = references.flatMap(_.lineReferences).toSet
    override def cached = CachedInferenceApplication.Direct(
      inference.id,
      inference.specifySubstitutions(substitutions),
      references.map(_.cached),
      isRearrangement,
      depth)
    override def conclusion = inference.conclusion.applySubstitutions(substitutions).get
  }

  case class Transformed(
      inference: Inference,
      substitutions: Substitutions,
      references: Seq[Reference],
      transformation: StatementDefinition,
      transformedPremises: Seq[Premise],
      transformedConclusion: Statement,
      transformationProof: Seq[Step],
      isRearrangement: Boolean,
      depth: Int)
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
      isRearrangement,
      depth)
    override def conclusion = transformedConclusion.applySubstitutions(substitutions).get
  }
}
