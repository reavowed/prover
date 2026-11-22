package net.prover.proving.rewrite

import net.prover.model._
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepContext, StepLike}
import net.prover.parsing.{KnownWordParser, Parser}
import net.prover.proving.derivation.DerivedStatement
import net.prover.proving.extraction.AppliedInferenceExtraction

/**
  * The equality (or other relation) used to rewrite a statement, along with how it was derived.
  */
sealed trait RewritePremise extends StepLike {
  /**
    * The inference that describes a rewrite using this premise, given the step (if any) that performs the rewrite.
    */
  def primaryInference(rewriteStep: Option[Step.AssertionStep]): Option[Inference.Summary]
}

object RewritePremise {
  /**
   * A premise that was auto-derived from existing statements.
   */
  case class Known(derivation: DerivedStatement) extends RewritePremise {
    override def statement: Statement = derivation.statement
    override def length: Int = derivation.length
    override def serializedLines: Seq[String] = derivation.serializedLines
    override def toProofSteps: Seq[Step] = derivation.toProofSteps
    override def primaryInference(rewriteStep: Option[Step.AssertionStep]): Option[Inference.Summary] = {
      rewriteStep.map(derivation.primaryInferenceOfRewrite).getOrElse(derivation.primaryInference)
    }
  }
  /**
    * A premise proved by an inference that was chosen explicitly, which therefore describes the rewrite.
    */
  case class ByInference(premises: Seq[DerivedStatement], extraction: AppliedInferenceExtraction) extends RewritePremise {
    override def statement: Statement = extraction.statement
    override def length: Int = premises.map(_.length).sum + extraction.length
    override def toProofSteps: Seq[Step] = Seq(Step.ElidedStep.ifNecessary((premises.flatMap(_.toProofSteps) :+ extraction.toProofStep), extraction.inference).get)
    override def primaryInference(rewriteStep: Option[Step.AssertionStep]): Option[Inference.Summary] = Some(extraction.inference.summary)
    override def serializedLines: Seq[String] = (premises.flatMap(_.serializedLines) ++ extraction.serializedLines).indentInLabelledBracesIfPresent("byInference")
  }

  def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[RewritePremise] = {
    val knownParser = DerivedStatement.parser.map(Known(_))
    val inferenceParser = KnownWordParser("byInference") {
      (for {
        premises <- DerivedStatement.listParser(stepContext.forChild(), implicitly).map(_._1)
        extraction <- AppliedInferenceExtraction.parser(stepContext.forChild().addSteps(premises.flatMap(_.toProofSteps)), implicitly)
      } yield ByInference(premises, extraction)).inBraces
    }
    KnownWordParser.select(Seq(knownParser, inferenceParser))
  }
}
