package net.prover.proving.derivation

import net.prover.model.*
import net.prover.model.definitions.KnownStatement
import net.prover.model.expressions.Statement
import net.prover.model.proof.Step
import net.prover.proving.derivation.DerivedStatement.Tier
import net.prover.proving.derivation.SimpleDerivationStep.{inference, toProofStep}
import net.prover.proving.extraction.{AppliedExtraction, AppliedInferenceExtraction, PremiseExtraction}

/**
  * A statement available at a point in a proof, along with the structure of how it was derived from statements that
  * were already known. Flattens to a list of proof steps in the same order the derivation was built in.
  */
sealed trait DerivedStatement {
  def statement: Statement
  private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep]
  def derivationSteps: Seq[SimpleDerivationStep] = allDerivationSteps.distinctBy(_.statement)
  /**
    * Every inference applied in this derivation, along with how much it contributes to describing the derivation as a
    * whole.
    */
  private[derivation] def tieredInferences(using ProvingContext): Seq[(Tier, Inference)]
  /**
    * The single inference application that describes the derivation as a whole, if there is one. This is the
    * inference of the most significant tier of inferences that the derivation uses, if that tier only uses a single
    * inference. In particular, if the derivation consists of a single step, it will be the inference of that step.
    */
  def primaryInference(using ProvingContext): Option[Inference.Summary] = {
    val inferencesByTier = tieredInferences.groupBy(_._1)
    Tier.values.iterator
      .flatMap(inferencesByTier.get)
      .nextOption()
      .flatMap(_.map(_._2).distinctBy(_.id).single)
      .map(_.summary)
  }

  def isEmpty: Boolean = allDerivationSteps.isEmpty
  def nonEmpty: Boolean = allDerivationSteps.nonEmpty
  def inferences: Set[Inference] = derivationSteps.map(_.inference).toSet
  def toProofSteps: Seq[Step] = derivationSteps.map(_.toProofStep)
  def toSimpleDerivation: SimpleDerivation = SimpleDerivation(derivationSteps)
  def toKnownStatement: KnownStatement = KnownStatement(statement, toSimpleDerivation)
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
      * An inference that does the real work of the derivation, e.g. "(a, b) = (c, d)" to "a = c".
      */
    case Substantive
    /**
      * An inference that only rearranges the logical structure of a statement, e.g. extracting a conjunct.
      */
    case Simplification
    /**
      * An inference that only restates a statement, e.g. reversing an equality or substituting a known value.
      */
    case Trivial
  }

  /**
    * A statement that is already available, e.g. a premise, a previous step, or a target.
    */
  case class Known(statement: Statement) extends DerivedStatement {
    override private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep] = Nil
    override private[derivation] def tieredInferences(using ProvingContext): Seq[(Tier, Inference)] = Nil
  }

  /**
    * A statement extracted from another one, e.g. by extracting a conjunct or specifying a universal statement.
    */
  case class Extracted(source: DerivedStatement, extraction: AppliedExtraction) extends DerivedStatement {
    override def statement: Statement = extraction.statement
    override private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep] = {
      source.allDerivationSteps ++ extraction.extractionSteps ++ extraction.chainedRewriteSteps
    }
    override private[derivation] def tieredInferences(using provingContext: ProvingContext): Seq[(Tier, Inference)] = {
      // A reversal may have been grouped into the extraction, and chained rewrites are joined on by transitivity
      def isTrivial(inference: Inference): Boolean = {
        provingContext.reversals.exists(_.inference.id == inference.id) ||
          provingContext.transitivities.exists(_.inference.id == inference.id)
      }
      source.tieredInferences ++ extraction.inferences.map(i => (if (isTrivial(i)) Tier.Trivial else Tier.Simplification) -> i)
    }
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
    override private[derivation] def tieredInferences(using ProvingContext): Seq[(Tier, Inference)] = {
      source.tieredInferences :+ (Tier.Substantive -> simplification.inference)
    }
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
    override private[derivation] def tieredInferences(using ProvingContext): Seq[(Tier, Inference)] = {
      source.tieredInferences ++ supportingPremise.toSeq.flatMap(_.tieredInferences) :+ (Tier.Substantive -> rewrite.inference)
    }
  }

  /**
    * A reversal of a symmetric statement, e.g. "a = b" to "b = a". Should only be applied after any simplifications.
    */
  case class Reversed(source: DerivedStatement, reversal: Step.AssertionStep) extends DerivedStatement {
    override def statement: Statement = reversal.statement
    override private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep] = source.allDerivationSteps :+ reversal
    override private[derivation] def tieredInferences(using ProvingContext): Seq[(Tier, Inference)] = {
      source.tieredInferences :+ (Tier.Trivial -> reversal.inference)
    }
  }

  /**
    * A statement proved by an inference with no premises, e.g. a fact or a simple assertion such as "a = a".
    */
  case class Asserted(assertion: AppliedInferenceExtraction) extends DerivedStatement {
    override def statement: Statement = assertion.statement
    override private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep] = Seq(assertion)
    override private[derivation] def tieredInferences(using ProvingContext): Seq[(Tier, Inference)] = {
      Seq(Tier.Substantive -> assertion.inference)
    }
  }

  /**
    * A statement built up from simpler components, found by breaking down the statement as a target, e.g.
    * "φ ∧ ψ" from "φ" and "ψ", or "a + b ∈ ℕ" from "a ∈ ℕ" and "b ∈ ℕ". The component derivations are in the order they
    * appear in the proof, which may not match the order of the inference's premises.
    */
  case class Composed(components: Seq[DerivedStatement], composition: SimpleDerivationStep) extends DerivedStatement {
    override def statement: Statement = composition.statement
    override private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep] = components.flatMap(_.allDerivationSteps) :+ composition
    override private[derivation] def tieredInferences(using provingContext: ProvingContext): Seq[(Tier, Inference)] = {
      // Combining a defined statement from its components is the counterpart of extracting them
      val tier = if (provingContext.statementDefinitionDeconstructions.exists(_.id == composition.inference.id)) Tier.Simplification else Tier.Substantive
      components.flatMap(_.tieredInferences) :+ (tier -> composition.inference)
    }
  }

  /**
    * A statement with a term replaced by an equal one, e.g. "x ∈ A" from "x ∈ domain(f)" and "A = domain(f)".
    */
  case class Substituted(source: DerivedStatement, equality: DerivedStatement, substitution: Step.AssertionStep) extends DerivedStatement {
    override def statement: Statement = substitution.statement
    override private[derivation] def allDerivationSteps: Seq[SimpleDerivationStep] = {
      (source.allDerivationSteps ++ equality.allDerivationSteps) :+ substitution
    }
    override private[derivation] def tieredInferences(using ProvingContext): Seq[(Tier, Inference)] = {
      (source.tieredInferences ++ equality.tieredInferences) :+ (Tier.Trivial -> substitution.inference)
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
    override private[derivation] def tieredInferences(using ProvingContext): Seq[(Tier, Inference)] = {
      (first.tieredInferences ++ second.tieredInferences) :+ (Tier.Trivial -> transitivity.inference)
    }
  }

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
}
