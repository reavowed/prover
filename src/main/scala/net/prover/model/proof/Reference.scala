package net.prover.model.proof

import net.prover.model._

sealed trait Reference {
  def referencedInferenceIds: Set[String]
  def factReferences: Set[Reference.ToFact] = Set.empty
  def cached: CachedReference
}

object Reference {
  sealed trait ToFact extends Reference {
    override def factReferences: Set[Reference.ToFact] = Set(this)
    def valueAndPath: (String, Seq[Int])
    def cached: CachedReference.ToFact
  }

  case class Direct(value: String) extends ToFact {
    override val referencedInferenceIds: Set[String] = Set.empty
    def withSuffix(suffix: String): Direct = Direct(value + suffix)
    override def valueAndPath: (String, Seq[Int]) = (value, Nil)
    override def cached = CachedReference.Direct(value)
  }

  sealed trait ApplyingInference extends Reference {
    def inferenceApplication: InferenceApplication
    override def factReferences: Set[Reference.ToFact] = inferenceApplication.directReferences
    override def referencedInferenceIds: Set[String] = inferenceApplication.referencedInferenceIds
  }

  case class Expansion(inferenceApplication: InferenceApplication) extends ApplyingInference {
    override def cached = CachedReference.Expansion(inferenceApplication.cached)
  }

  case class Simplification(
      inference: Inference,
      substitutions: Substitutions,
      inferenceReference: Reference.ToFact,
      simplificationPath: Seq[Int],
      depth: Int)
    extends ToFact
  {
    override def referencedInferenceIds: Set[String] = inferenceReference.referencedInferenceIds
    override def valueAndPath: (String, Seq[Int]) = inferenceReference.valueAndPath.mapRight(simplificationPath ++ _)
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
