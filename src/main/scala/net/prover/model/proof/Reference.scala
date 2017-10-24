package net.prover.model.proof

import net.prover.model._

sealed trait Reference {
  def lineReferences: Set[(String, Seq[Int])]
  def referencedInferenceIds: Set[String]
  def cached: CachedReference
}

object Reference {
  sealed trait Compoundable extends Reference {
    def add(other: Direct) = Compound(other, this)
    def cached: CachedReference.Compoundable
  }
  sealed trait ToSingleLine extends Reference {
    def lineReference: (String, Seq[Int])
    override def lineReferences = Set(lineReference)
    def cached: CachedReference.ToSingleLine
  }

  case class Direct(value: String) extends Compoundable with ToSingleLine {
    override def referencedInferenceIds = Set.empty
    def lineReference = (value, Seq.empty[Int])
    override def lineReferences = Set(lineReference)
    override def cached = CachedReference.Direct(value)
    def withSuffix(suffix: String): Direct = Direct(value + suffix)
  }

  case class Compound(first: Direct, other: Compoundable) extends Compoundable {
    override def lineReferences = other.lineReferences + first.lineReference
    def referencedInferenceIds = first.referencedInferenceIds ++ other.referencedInferenceIds
    def cached = CachedReference.Compound(first.cached, other.cached)
  }

  sealed trait ApplyingInference extends Reference {
    def inferenceApplication: InferenceApplication
    override def lineReferences = inferenceApplication.lineReferences
    override def referencedInferenceIds = inferenceApplication.referencedInferenceIds
  }

  case class Expansion(inferenceApplication: InferenceApplication) extends ApplyingInference {
    override def cached = CachedReference.Expansion(inferenceApplication.cached)
  }

  case class Simplification(
      inference: Inference,
      substitutions: Substitutions,
      inferenceReference: Reference.ToSingleLine,
      simplificationPath: Seq[Int],
      depth: Int)
    extends ToSingleLine
  {
    override def referencedInferenceIds = inferenceReference.referencedInferenceIds
    override def lineReference = inferenceReference.lineReference.mapRight(simplificationPath ++ _)
    override def cached = CachedReference.Simplification(
      inference.id,
      inference.specifySubstitutions(substitutions),
      inferenceReference.cached,
      simplificationPath,
      depth)
  }

  case class Elided(inferenceApplication: InferenceApplication) extends ApplyingInference {
    override def cached = CachedReference.Elided(inferenceApplication.cached)
  }

  def nextReference(baseReference: Option[Reference.Direct], suffix: String): Reference.Direct = {
    baseReference match {
      case Some(value) =>
        value.withSuffix("." + suffix)
      case None =>
        Reference.Direct(suffix)
    }
  }
}
