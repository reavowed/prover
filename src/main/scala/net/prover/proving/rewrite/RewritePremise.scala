package net.prover.proving.rewrite

import net.prover.model.Inference
import net.prover.model.definitions.KnownStatement
import net.prover.model.proof.Step
import net.prover.proving.extraction.AppliedInferenceExtraction

sealed trait RewritePremise {
  def length: Int
  def toProofSteps: Seq[Step]
  def explicitInference: Option[Inference.Summary] = None
  def fallbackInference: Option[Inference.Summary] = None
}

object RewritePremise {
  case class Known(knownStatement: KnownStatement) extends RewritePremise {
    override def length: Int = knownStatement.derivation.length
    override def toProofSteps: Seq[Step] = knownStatement.derivation.toProofSteps
    override def fallbackInference: Option[Inference.Summary] = knownStatement.derivation.inferences.single.map(_.summary)
  }
  case class ByInference(premises: Seq[KnownStatement], extraction: AppliedInferenceExtraction) extends RewritePremise {
    override def length: Int = premises.map(_.derivation.length).sum + extraction.length
    override def toProofSteps: Seq[Step] = Seq(Step.ElidedStep.ifNecessary((premises.flatMap(_.derivation.toProofSteps) :+ extraction.toStep).distinctBy(_.statement), extraction.inference).get)
    override def explicitInference: Option[Inference.Summary] = Some(extraction.inference.summary)
  }
}