package net.prover.proving.rewrite

import net.prover.model._
import net.prover.model.definitions.KnownStatement
import net.prover.model.proof.{Step, StepContext, StepLike}
import net.prover.proving.extraction.AppliedInferenceExtraction

sealed trait RewritePremise extends StepLike.Wrapper {
  def length: Int
  def toProofSteps: Seq[Step]
  def explicitInference: Option[Inference.Summary] = None
  def fallbackInference: Option[Inference.Summary] = None
}

object RewritePremise {
  case class Known(knownStatement: KnownStatement) extends RewritePremise {
    override def substeps: Seq[StepLike] = Seq(knownStatement)
    override def toProofSteps: Seq[Step] = knownStatement.derivation.toProofSteps
    override def fallbackInference: Option[Inference.Summary] = knownStatement.derivation.inferences.single.map(_.summary)
  }
  case class ByInference(premises: Seq[KnownStatement], extraction: AppliedInferenceExtraction) extends RewritePremise {
    override def substeps: Seq[StepLike] = premises :+ extraction
    override def toProofSteps: Seq[Step] = Seq(Step.ElidedStep.ifNecessary((premises.flatMap(_.derivation.toProofSteps) :+ extraction.toStep), extraction.inference).get)
    override def explicitInference: Option[Inference.Summary] = Some(extraction.inference.summary)
    override def serializedLines: Seq[String] = super.serializedLines.indentInLabelledBracesIfPresent("byInference")
  }

  def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[RewritePremise] = {
    KnownStatement.parser.mapMap(Known(_))
      .orElse(
        Parser.requiredWord("byInference").flatMap(_ =>
          (for {
            premises <- KnownStatement.listParser(stepContext.forChild(), implicitly).map(_._1)
            extraction <- AppliedInferenceExtraction.parser(stepContext.forChild().addSteps(premises.flatMap(_.derivation.toProofSteps)), implicitly)
          } yield ByInference(premises, extraction)).inBraces
        ))
  }
}