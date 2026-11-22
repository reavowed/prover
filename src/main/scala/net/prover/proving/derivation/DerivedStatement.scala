package net.prover.proving.derivation

import net.prover.model.*
import net.prover.model.expressions.Statement
import net.prover.model.proof.{Step, StepContext, StepLike}
import net.prover.parsing.{KnownWordParser, Parser}
import net.prover.proving.derivation.DerivedStatement.Tier
import net.prover.proving.derivation.SimpleDerivationStep.{inference, toProofStep}
import net.prover.proving.extraction.{AppliedExtraction, AppliedExtractionStep, AppliedInferenceExtraction, PremiseExtraction}

/**
  * A statement available at a point in a proof, along with the structure of how it was derived from statements that
  * were already known. Flattens to a list of proof steps in the same order the derivation was built in, omitting any
  * step whose statement was already derived earlier in the derivation.
  */
sealed trait DerivedStatement {
  def statement: Statement
  private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep]
  def derivationSteps: Seq[SimpleDerivationStep] = allDerivationSteps.distinctBy(_.statement)
  /**
    * Every inference applied in this derivation, along with how much it contributes to describing the derivation as a
    * whole.
    */
  private[derivation] def tieredInferences: Seq[(Tier, Inference)]
  /**
    * The single inference application that describes the derivation as a whole, if there is one. This is the
    * inference of the most significant tier of inferences that the derivation uses, if that tier only uses a single
    * inference. In particular, if the derivation consists of a single step, it will be the inference of that step.
    */
  def primaryInference: Option[Inference.Summary] = primaryInferenceWith(Nil)
  /**
    * The primary inference of rewriting another statement using this one, e.g. by substitution of equals. The rewrite
    * itself only restates the other statement.
    */
  def primaryInferenceOfRewrite(rewriteStep: Step.AssertionStep): Option[Inference.Summary] = {
    primaryInferenceWith(Seq(Tier.Rewrite -> rewriteStep.inference))
  }
  protected def primaryInferenceWith(additionalInferences: Seq[(Tier, Inference)]): Option[Inference.Summary] = {
    DerivedStatement.highestTierInference(tieredInferences ++ additionalInferences)
  }
  /**
    * The statements that this derivation starts from, i.e. those that were already known.
    */
  def leafStatements: Seq[Statement]
  def serializedLines: Seq[String]

  def isEmpty: Boolean = allDerivationSteps.isEmpty
  def nonEmpty: Boolean = allDerivationSteps.nonEmpty
  def length: Int = toProofSteps.map(_.length).sum
  def inferences: Set[Inference] = derivationSteps.map(_.inference).toSet
  def toProofSteps: Seq[Step] = derivationSteps.map(_.toProofStep)
  /**
    * Uses the given statement (equal to this one, but possibly with different bound variable names) if this statement
    * was already known.
    */
  def withKnownStatement(knownStatement: Statement): DerivedStatement = this match {
    case _: DerivedStatement.Known => DerivedStatement.Known(knownStatement)
    case _ => this
  }
}

object DerivedStatement {
  /**
    * How much an inference application contributes to describing a derivation, in descending order of priority.
    */
  private[derivation] enum Tier {
    /**
      * An inference that builds a statement about terms from simpler ones, e.g. "a + b ∈ ℕ" from "a ∈ ℕ" and "b ∈ ℕ".
      */
    case Composition
    /**
      * An inference that rewrites a relation to a different one, e.g. "a ∈ ℤ⁺" to "0 < a".
      */
    case RelationRewrite
    /**
      * An inference with no premises, e.g. "0 ∈ ℕ".
      */
    case Fact
    /**
      * An inference that simplifies a relation, e.g. "(a, b) = (c, d)" to "a = c".
      */
    case Simplification
    /**
      * An inference that only extracts from or builds up the logical structure of a statement, e.g. extracting a
      * conjunct.
      */
    case Extraction
    /**
      * An inference that only restates a statement, e.g. reversing an equality or substituting a known value.
      */
    case Rewrite
  }

  private def highestTierInference(tieredInferences: Seq[(Tier, Inference)]): Option[Inference.Summary] = {
    val inferencesByTier = tieredInferences.groupBy(_._1)
    Tier.values.iterator
      .flatMap(inferencesByTier.get)
      .nextOption()
      .flatMap(_.map(_._2).distinctBy(_.id).single)
      .map(_.summary)
  }

  private def stepLines(step: SimpleDerivationStep): Seq[String] = (step: StepLike).serializedLines

  private def extractionTieredInferences(extraction: AppliedExtraction): Seq[(Tier, Inference)] = {
    extraction.extractionInferences.map(Tier.Extraction -> _) ++
      extraction.reversalStep.map(Tier.Rewrite -> _.inference) ++
      extraction.chainedRewrites.flatMap(r => Seq(Tier.Extraction -> r.rewriteStep.inference, Tier.Rewrite -> r.chainingStep.inference))
  }

  /**
    * The single inference used, other than rewrites, if there is one. Used for a statement built from parts that were
    * derived separately, which is only described by its own inference if nothing other than rewrites was needed to
    * derive those parts.
    */
  private def singleInferenceIgnoringRewrites(tieredInferences: Seq[(Tier, Inference)]): Option[Inference.Summary] = {
    tieredInferences
      .filter(_._1 != Tier.Rewrite)
      .map(_._2)
      .distinctBy(_.id)
      .single
      .map(_.summary)
  }

  /**
    * The primary inference of an extraction from an existing statement, whose extraction premises were derived
    * separately. As with a combination, the extraction only describes the whole if nothing other than rewrites was
    * needed to derive the premises.
    */
  def primaryInferenceOfExtraction(premiseDerivations: Seq[DerivedStatement], extraction: AppliedExtraction): Option[Inference.Summary] = {
    singleInferenceIgnoringRewrites(premiseDerivations.flatMap(_.tieredInferences) ++ extractionTieredInferences(extraction))
  }

  /**
    * A statement that is already available, e.g. a premise, a previous step, or a target.
    */
  case class Known(statement: Statement) extends DerivedStatement {
    override private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep] = Nil
    override private[derivation] def tieredInferences: Seq[(Tier, Inference)] = Nil
    override def leafStatements: Seq[Statement] = Seq(statement)
    override def serializedLines: Seq[String] = Seq(s"$KnownLabel ${statement.serialized}")
  }

  /**
    * A statement extracted from another one, e.g. by extracting a conjunct or specifying a universal statement.
    */
  case class Extracted(source: DerivedStatement, extraction: AppliedExtraction) extends DerivedStatement {
    override def statement: Statement = extraction.statement
    override private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep] = {
      source.allDerivationSteps ++ extraction.extractionSteps ++ extraction.chainedRewriteSteps
    }
    override private[derivation] def tieredInferences: Seq[(Tier, Inference)] = {
      source.tieredInferences ++ extractionTieredInferences(extraction)
    }
    override def leafStatements: Seq[Statement] = source.leafStatements
    override def serializedLines: Seq[String] = (source.serializedLines ++ extraction.serializedLines).indentInLabelledBracesIfPresent(ExtractedLabel)
  }
  object Extracted {
    def ifNecessary(source: DerivedStatement, extraction: AppliedExtraction): DerivedStatement = {
      if (extraction.nonEmpty) Extracted(source, extraction) else source
    }
  }

  /**
    * A binary relation statement simplified by an inference that reduces the complexity of its right-hand side, e.g.
    * "(a, b) = (c, d)" to "a = c".
    */
  case class Simplified(source: DerivedStatement, simplification: AppliedInferenceExtraction) extends DerivedStatement {
    override def statement: Statement = simplification.statement
    override private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep] = source.allDerivationSteps :+ simplification
    override private[derivation] def tieredInferences: Seq[(Tier, Inference)] = {
      source.tieredInferences :+ (Tier.Simplification -> simplification.inference)
    }
    override def leafStatements: Seq[Statement] = source.leafStatements
    override def serializedLines: Seq[String] = (source.serializedLines ++ simplification.serializedLines).indentInLabelledBracesIfPresent(SimplifiedLabel)
  }

  /**
    * A binary relation statement rewritten to a different relation, e.g. "a ∈ ℤ+" to "0 < a", possibly with the help of
    * a supporting premise.
    */
  case class RelationRewritten(source: DerivedStatement, supportingPremise: Option[DerivedStatement], rewrite: AppliedInferenceExtraction) extends DerivedStatement {
    override def statement: Statement = rewrite.statement
    override private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep] = {
      source.allDerivationSteps ++ supportingPremise.toSeq.flatMap(_.allDerivationSteps) :+ rewrite
    }
    override private[derivation] def tieredInferences: Seq[(Tier, Inference)] = {
      source.tieredInferences ++ supportingPremise.toSeq.flatMap(_.tieredInferences) :+ (Tier.RelationRewrite -> rewrite.inference)
    }
    override def leafStatements: Seq[Statement] = source.leafStatements ++ supportingPremise.toSeq.flatMap(_.leafStatements)
    override def serializedLines: Seq[String] = {
      (source.serializedLines ++ supportingPremise.toSeq.flatMap(_.serializedLines) ++ rewrite.serializedLines).indentInLabelledBracesIfPresent(RelationRewrittenLabel)
    }
  }

  /**
    * A reversal of a symmetric statement, e.g. "a = b" to "b = a". Should only be applied after any simplifications.
    */
  case class Reversed(source: DerivedStatement, reversal: Step.AssertionStep) extends DerivedStatement {
    override def statement: Statement = reversal.statement
    override private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep] = source.allDerivationSteps :+ reversal
    override private[derivation] def tieredInferences: Seq[(Tier, Inference)] = {
      source.tieredInferences :+ (Tier.Rewrite -> reversal.inference)
    }
    override def leafStatements: Seq[Statement] = source.leafStatements
    override def serializedLines: Seq[String] = (source.serializedLines ++ reversal.serializedLines).indentInLabelledBracesIfPresent(ReversedLabel)
  }

  /**
    * A statement proved by an inference with no premises, e.g. a fact or a simple assertion such as "a = a".
    */
  case class Asserted(assertion: AppliedInferenceExtraction) extends DerivedStatement {
    override def statement: Statement = assertion.statement
    override private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep] = Seq(assertion)
    override private[derivation] def tieredInferences: Seq[(Tier, Inference)] = {
      Seq(Tier.Fact -> assertion.inference)
    }
    override def leafStatements: Seq[Statement] = Nil
    override def serializedLines: Seq[String] = assertion.serializedLines.indentInLabelledBracesIfPresent(AssertedLabel)
  }

  /**
    * A statement combined from its components by its logical structure alone, found by breaking down the statement as a
    * target, e.g. "φ ∧ ψ" from "φ" and "ψ". This is the counterpart of extracting the components. The component
    * derivations are in the order they appear in the proof, which may not match the order of the inference's premises.
    */
  case class Combined(components: Seq[DerivedStatement], combination: Step.AssertionStep) extends DerivedStatement {
    override def statement: Statement = combination.statement
    override private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep] = components.flatMap(_.allDerivationSteps) :+ combination
    override private[derivation] def tieredInferences: Seq[(Tier, Inference)] = {
      components.flatMap(_.tieredInferences) :+ (Tier.Extraction -> combination.inference)
    }
    // A combination only describes the derivation if nothing other than rewrites had to happen to find its components.
    // This only applies to the combination as a whole - as part of a larger derivation, it contributes its inferences
    // as normal.
    override protected def primaryInferenceWith(additionalInferences: Seq[(Tier, Inference)]): Option[Inference.Summary] = {
      singleInferenceIgnoringRewrites(tieredInferences ++ additionalInferences)
    }
    override def leafStatements: Seq[Statement] = components.flatMap(_.leafStatements)
    override def serializedLines: Seq[String] = (components.flatMap(_.serializedLines) ++ combination.serializedLines).indentInLabelledBracesIfPresent(CombinedLabel)
  }

  /**
    * A statement built up from simpler components, found by breaking down the statement as a target, e.g. "a + b ∈ ℕ"
    * from "a ∈ ℕ" and "b ∈ ℕ". The component derivations are in the order they appear in the proof, which may not
    * match the order of the inference's premises.
    */
  case class Composed(components: Seq[DerivedStatement], composition: SimpleDerivationStep) extends DerivedStatement {
    override def statement: Statement = composition.statement
    override private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep] = components.flatMap(_.allDerivationSteps) :+ composition
    override private[derivation] def tieredInferences: Seq[(Tier, Inference)] = {
      components.flatMap(_.tieredInferences) :+ (Tier.Composition -> composition.inference)
    }
    override def leafStatements: Seq[Statement] = components.flatMap(_.leafStatements)
    override def serializedLines: Seq[String] = (components.flatMap(_.serializedLines) ++ stepLines(composition)).indentInLabelledBracesIfPresent(ComposedLabel)
  }

  /**
    * A statement with a term replaced by an equal one, e.g. "x ∈ A" from "x ∈ domain(f)" and "A = domain(f)".
    */
  case class Substituted(source: DerivedStatement, equality: DerivedStatement, substitution: Step.AssertionStep) extends DerivedStatement {
    override def statement: Statement = substitution.statement
    override private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep] = {
      (source.allDerivationSteps ++ equality.allDerivationSteps) :+ substitution
    }
    override private[derivation] def tieredInferences: Seq[(Tier, Inference)] = {
      (source.tieredInferences ++ equality.tieredInferences) :+ (Tier.Rewrite -> substitution.inference)
    }
    override def leafStatements: Seq[Statement] = source.leafStatements ++ equality.leafStatements
    override def serializedLines: Seq[String] = {
      (source.serializedLines ++ equality.serializedLines ++ substitution.serializedLines).indentInLabelledBracesIfPresent(SubstitutedLabel)
    }
  }

  /**
    * A relation chained from two others, e.g. "a = c" from "a = b" and "b = c".
    */
  case class Transitive(first: DerivedStatement, second: DerivedStatement, transitivity: Step.AssertionStep) extends DerivedStatement {
    override def statement: Statement = transitivity.statement
    override private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep] = {
      (first.allDerivationSteps ++ second.allDerivationSteps) :+ transitivity
    }
    override private[derivation] def tieredInferences: Seq[(Tier, Inference)] = {
      (first.tieredInferences ++ second.tieredInferences) :+ (Tier.Rewrite -> transitivity.inference)
    }
    override def leafStatements: Seq[Statement] = first.leafStatements ++ second.leafStatements
    override def serializedLines: Seq[String] = {
      (first.serializedLines ++ second.serializedLines ++ transitivity.serializedLines).indentInLabelledBracesIfPresent(TransitiveLabel)
    }
  }

  private val KnownLabel = "known"
  private val ExtractedLabel = "extracted"
  private val SimplifiedLabel = "simplified"
  private val RelationRewrittenLabel = "relationRewritten"
  private val ReversedLabel = "reversed"
  private val AssertedLabel = "asserted"
  private val CombinedLabel = "combined"
  private val ComposedLabel = "composed"
  private val SubstitutedLabel = "substituted"
  private val TransitiveLabel = "transitive"

  /**
    * The derivation of a statement extracted from a premise. A final reversal is split out into its own node where the
    * extraction allows it, so that it is distinguished from the extraction itself.
    */
  def fromPremiseExtraction(premiseExtraction: PremiseExtraction)(using provingContext: ProvingContext): DerivedStatement = {
    val extractionDetails = premiseExtraction.extractionDetails
    val source = Known(extractionDetails.mainPremise)
    val extraction = extractionDetails.finalise
    def withSeparateReversal = for {
      reversalStep <- extractionDetails.reversalStep
      if extractionDetails.leftRewrite.isEmpty && extractionDetails.rightRewrite.isEmpty
      unreversedExtraction = extractionDetails.copy(reversalStep = None).finalise
      // Grouping by definition may have absorbed the reversal into a definition deconstruction
      if extraction.extractionSteps == unreversedExtraction.extractionSteps :+ reversalStep
    } yield Reversed(Extracted.ifNecessary(source, unreversedExtraction), reversalStep)
    withSeparateReversal getOrElse Extracted.ifNecessary(source, extraction)
  }

  implicit class SeqOps(derivedStatements: Seq[DerivedStatement]) {
    def deduplicate: Seq[DerivedStatement] = derivedStatements.distinctBy(_.statement)
  }

  /**
    * Tracks the step context while moving through a derivation in order, adding each step to the context unless a step
    * with the same statement has already been added. This matches the proof steps that the derivation flattens to.
    */
  case class DerivationContext(stepContext: StepContext, derivedStatements: Set[Statement]) {
    def add(step: SimpleDerivationStep): DerivationContext = {
      if (derivedStatements.contains(step.statement))
        this
      else
        DerivationContext(stepContext.addStep(step.toProofStep), derivedStatements + step.statement)
    }
    def addAll(steps: Seq[SimpleDerivationStep]): DerivationContext = steps.foldLeft(this)(_.add(_))
  }
  object DerivationContext {
    def apply(stepContext: StepContext): DerivationContext = DerivationContext(stepContext, Set.empty)
  }

  def parser(implicit stepContext: StepContext, provingContext: ProvingContext): KnownWordParser[DerivedStatement] = {
    parserInContext(DerivationContext(stepContext)).map(_._1)
  }
  /**
    * Parses a list of separate derivations, each of which is flattened on its own.
    */
  def listParser(implicit stepContext: StepContext, provingContext: ProvingContext): Parser[(Seq[DerivedStatement], StepContext)] = {
    StepLike.listParser[DerivedStatement](parser(_, implicitly), (sc, d) => sc.addSteps(d.toProofSteps))
  }

  private def parserInContext(context: DerivationContext)(implicit provingContext: ProvingContext): KnownWordParser[(DerivedStatement, DerivationContext)] = {
    def statementParser = Statement.parser(ExpressionParsingContext.atStep(context.stepContext, provingContext))
    def stepParser[T <: SimpleDerivationStep](getParser: StepContext => Parser[T])(context: DerivationContext): Parser[(T, DerivationContext)] = {
      getParser(context.stepContext).map(step => (step, context.add(step)))
    }
    def assertionParser(context: DerivationContext) = stepParser(Step.AssertionStep.parser(_, provingContext))(context)
    def inferenceExtractionParser(context: DerivationContext) = stepParser(AppliedInferenceExtraction.parser(_, provingContext))(context)
    def nodeParser(context: DerivationContext) = parserInContext(context)
    def nodesParser(context: DerivationContext): Parser[(Seq[DerivedStatement], DerivationContext)] = {
      KnownWordParser.mapFoldWhileDefined(context)(nodeParser)
    }
    def withSource(label: String)(f: (DerivedStatement, DerivationContext) => Parser[(DerivedStatement, DerivationContext)]) = {
      KnownWordParser(label) {
        nodeParser(context).flatMap(f.tupled).inBraces
      }
    }

    val known = KnownWordParser(KnownLabel)(statementParser.map(s => (Known(s), context)))
    val extracted = withSource(ExtractedLabel) { (source, context) =>
      extractionParser(context).map(_.mapLeft(Extracted(source, _)))
    }
    val simplified = withSource(SimplifiedLabel) { (source, context) =>
      inferenceExtractionParser(context).map(_.mapLeft(Simplified(source, _)))
    }
    val relationRewritten = withSource(RelationRewrittenLabel) { (source, context) =>
      for {
        (supportingPremise, contextAfterSupportingPremise) <- nodeParser(context).optional.map(_.map(_.mapLeft(Some(_))).getOrElse((None, context)))
        (rewrite, contextAfterRewrite) <- inferenceExtractionParser(contextAfterSupportingPremise)
      } yield (RelationRewritten(source, supportingPremise, rewrite), contextAfterRewrite)
    }
    val reversed = withSource(ReversedLabel) { (source, context) =>
      assertionParser(context).map(_.mapLeft(Reversed(source, _)))
    }
    val asserted = KnownWordParser(AssertedLabel) {
      inferenceExtractionParser(context).map(_.mapLeft(Asserted(_))).inBraces
    }
    val combined = KnownWordParser(CombinedLabel) {
      (for {
        (components, contextAfterComponents) <- nodesParser(context)
        (combination, contextAfterCombination) <- assertionParser(contextAfterComponents)
      } yield (Combined(components, combination), contextAfterCombination)).inBraces
    }
    val composed = KnownWordParser(ComposedLabel) {
      (for {
        (components, contextAfterComponents) <- nodesParser(context)
        (composition, contextAfterComposition) <- stepParser(SimpleDerivationStep.parser(_, provingContext))(contextAfterComponents)
      } yield (Composed(components, composition), contextAfterComposition)).inBraces
    }
    val substituted = withSource(SubstitutedLabel) { (source, context) =>
      for {
        (equality, contextAfterEquality) <- nodeParser(context)
        (substitution, contextAfterSubstitution) <- assertionParser(contextAfterEquality)
      } yield (Substituted(source, equality, substitution), contextAfterSubstitution)
    }
    val transitive = withSource(TransitiveLabel) { (first, context) =>
      for {
        (second, contextAfterSecond) <- nodeParser(context)
        (transitivity, contextAfterTransitivity) <- assertionParser(contextAfterSecond)
      } yield (Transitive(first, second, transitivity), contextAfterTransitivity)
    }
    KnownWordParser.select(Seq(known, extracted, simplified, relationRewritten, reversed, asserted, combined, composed, substituted, transitive))
  }

  /**
    * Parses the steps of an extraction, as written by [[AppliedExtraction.serializedLines]].
    */
  private def extractionParser(context: DerivationContext)(implicit provingContext: ProvingContext): Parser[(AppliedExtraction, DerivationContext)] = {
    for {
      (extractionSteps, contextAfterExtraction) <- Parser.mapFoldWhileDefined[AppliedExtractionStep, DerivationContext](context) { (_, context) =>
        AppliedExtractionStep.parser(context.stepContext, provingContext).optional.mapMap(step => step -> context.add(step))
      }
      (rewriteSteps, contextAfterRewrites) <- KnownWordParser("chainedRewrite") {
        Parser.mapFoldWhileDefined[Step.AssertionStep, DerivationContext](contextAfterExtraction) { (_, context) =>
          Step.AssertionStep.parser(context.stepContext, provingContext).optional.mapMap(step => step -> context.add(step))
        }.inBraces
      }.optional.map(_.getOrElse((Nil, contextAfterExtraction)))
    } yield (
      AppliedExtraction(extractionSteps, AppliedExtraction.pairChainedRewriteSteps(rewriteSteps), AppliedExtraction.endsInReversal(extractionSteps)),
      contextAfterRewrites)
  }
}
