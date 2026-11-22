package net.prover.proving.extraction

import net.prover.model.*
import net.prover.model.proof.{Step, StepContext, StepLike}
import net.prover.parsing.{KnownWordParser, Parser}
import net.prover.proving.derivation.DefinitionDeconstruction
import net.prover.proving.extraction.PartiallyAppliedExtraction.ChainedRewriteStep

/**
  * The steps of an extraction, grouped as they appear in a proof.
  *
  * @param extractionSteps The extraction steps, with any definition deconstructions grouped together. If the extraction
  *                        ends in a reversal, it is the final step, which may have been grouped into a final definition
  *                        deconstruction.
  * @param chainedRewrites Any rewrites of the result, each joined on by a transitivity step.
  * @param endsInReversal Whether the final extraction step is a reversal of the extracted statement.
  */
case class AppliedExtraction(
  extractionSteps: Seq[AppliedExtractionStep],
  chainedRewrites: Seq[ChainedRewriteStep],
  endsInReversal: Boolean
) extends StepLike.Wrapper {
  override def substeps: Seq[StepLike] = extractionSteps ++ chainedRewriteSteps
  def chainedRewriteSteps: Seq[Step.AssertionStep] = chainedRewrites.flatMap(_.substeps)
  def nonEmpty: Boolean = substeps.nonEmpty
  private def extractionAssertions: Seq[Step.AssertionStep] = extractionSteps flatMap {
    case step: Step.AssertionStep => Seq(step)
    case step: DefinitionDeconstruction => step.substeps
  }
  def inferences: Seq[Inference] = (extractionAssertions ++ chainedRewriteSteps).map(_.inference)
  def reversalStep: Option[Step.AssertionStep] = if (endsInReversal) extractionAssertions.lastOption else None
  /**
    * The inferences of the extraction steps, not including any reversal or chained rewrites.
    */
  def extractionInferences: Seq[Inference] = {
    (if (endsInReversal) extractionAssertions.dropRight(1) else extractionAssertions).map(_.inference)
  }

  override def serializedLines: Seq[String] = {
    extractionSteps.flatMap(_.serializedLines) ++
      Seq(chainedRewriteSteps)
        .filter(_.nonEmpty)
        .flatMap(_.flatMap(_.serializedLines).indentInLabelledBracesIfPresent("chainedRewrite"))
  }
}
object AppliedExtraction {
  val empty: AppliedExtraction = AppliedExtraction(Nil, Nil, false)
  /**
    * An extraction with no reversal or chained rewrites.
    */
  def fromSimpleExtraction(extractionSteps: Seq[Step.AssertionStep]): AppliedExtraction = {
    AppliedExtraction(extractionSteps, Nil, false)
  }
  /**
    * Whether the last of the given extraction steps is a reversal. Since extraction inferences always reduce the
    * complexity of the statement they apply to, a reversal can only ever be added at the end of an extraction.
    */
  def endsInReversal(extractionSteps: Seq[AppliedExtractionStep])(implicit provingContext: ProvingContext): Boolean = {
    val lastAssertion = extractionSteps.lastOption.map {
      case step: Step.AssertionStep => step
      case step: DefinitionDeconstruction => step.substeps.last
    }
    lastAssertion.exists(step => provingContext.reversals.exists(_.inference.id == step.inference.id))
  }
  def parser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[AppliedExtraction] = {
    for {
      extractionStepsAndContext <- KnownWordParser.mapFoldWhileDefined(stepContext) { stepContext =>
        AppliedExtractionStep.parser(stepContext, implicitly)
          .map(step => step -> stepContext.addStep(step.toProofStep))
      }
      (extractionSteps, stepContext) = extractionStepsAndContext
      rewriteSteps <- KnownWordParser("chainedRewrite") {
        Parser.mapFoldWhileDefined[Step.AssertionStep, StepContext](stepContext) { (_, stepContext) =>
          Step.AssertionStep.parser(stepContext, implicitly).optional.mapMap(step => step -> stepContext.addStep(step))
        }.inBraces.map(_._1)
      }.optional.map(_.getOrElse(Nil))
    } yield AppliedExtraction(extractionSteps, pairChainedRewriteSteps(rewriteSteps), endsInReversal(extractionSteps))
  }
  def pairChainedRewriteSteps(steps: Seq[Step.AssertionStep]): Seq[ChainedRewriteStep] = {
    steps.grouped(2).map {
      case Seq(rewriteStep, chainingStep) => ChainedRewriteStep(rewriteStep, chainingStep)
      case _ => throw new Exception("Chained rewrite steps must come in pairs")
    }.toSeq
  }
}

type AppliedExtractionStep = Step.AssertionStep | DefinitionDeconstruction

object AppliedExtractionStep {
  def apply(step: Step.AssertionOrExtraction): AppliedExtractionStep = step match {
    case step: Step.AssertionStep => step
    case step: Step.InferenceExtractionStep => DefinitionDeconstruction(
      step.inferenceExtraction.assertionStep,
      step.inferenceExtraction.extraction.extractionSteps.collect { case step: Step.AssertionStep => step },
      step.inferenceExtraction.extraction.endsInReversal)
  }

  def parser(implicit stepContext: StepContext, provingContext: ProvingContext): KnownWordParser[AppliedExtractionStep] = {
    val assertionParser = Step.AssertionStep.parser
    val definitionParser = KnownWordParser("deconstruction") {
      val innerContext = stepContext.forChild()
      (for {
        deconstructionStep <- Step.AssertionStep.parser(innerContext, implicitly)
        stepContextAfterDeconstructionStep = innerContext.addStep(deconstructionStep)
        additionalSteps <- Step.listParser(stepContext =>
          Step.AssertionStep.parser(stepContext, implicitly)
        )(stepContextAfterDeconstructionStep).map(_._1)
      } yield DefinitionDeconstruction(deconstructionStep, additionalSteps, AppliedExtraction.endsInReversal(additionalSteps))).inBraces
    }
    KnownWordParser.select(Seq(assertionParser, definitionParser))
  }
}
