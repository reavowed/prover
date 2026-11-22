package net.prover

import net.prover.model.{Inference, ProvingContext, Substitutions}
import net.prover.model.expressions.{Statement, Term, TermVariable}
import net.prover.model.proof.{Step, SubstitutionContext}
import net.prover.model.TestDefinitions._
import net.prover.model.definitions.KnownStatement
import net.prover.model.proof.Step.RewriteStep
import net.prover.proving.derivation.{DerivedStatement, SimpleDerivation, SimpleDerivationStep}
import net.prover.proving.extraction.{AppliedExtraction, AppliedExtractionStep, AppliedInferenceExtraction}
import net.prover.proving.rewrite.RewritePremise

import scala.language.implicitConversions

trait StepHelpers {
  def assertion(inference: Inference, statements: Seq[Statement], terms: Seq[Term]): SubstitutionContext => Step.AssertionStep = { substitutionContext =>
    Step.AssertionStep.forInference(inference, Substitutions(statements, terms))(substitutionContext).get
  }

  def generalization(
    variableName: String,
    steps: SubstitutionContext => Seq[Step]
  ): SubstitutionContext => Step.GeneralizationStep = { sc =>
    Step.GeneralizationStep(variableName, steps(SubstitutionContext.withExtraParameter(sc)), GeneralizationDefinition)
  }
  def deduction(
    antecedent: Statement,
    steps: SubstitutionContext => Seq[Step]
  ): SubstitutionContext => Step.DeductionStep = { sc =>
    Step.DeductionStep(antecedent, steps(sc), DeductionDefinition)
  }
  def target(
    statement: Statement
  ): SubstitutionContext => Step.TargetStep = { _ =>
    Step.TargetStep(statement)
  }
  def elided(
    inference: Inference,
    steps: SubstitutionContext => Seq[Step]
  ): SubstitutionContext => Step.ElidedStep = { sc =>
    Step.ElidedStep(steps(sc), Some(inference.summary), None)
  }
  def elided(
    description: String,
    steps: SubstitutionContext => Seq[Step]
  ): SubstitutionContext => Step.ElidedStep = { sc =>
    Step.ElidedStep(steps(sc), None, Some(description))
  }
  def existingStatementExtraction(
    steps: SubstitutionContext => Seq[Step.AssertionStep])(
    implicit provingContext: ProvingContext
  ): SubstitutionContext => Step.ExistingStatementExtractionStep = { sc =>
    Step.ExistingStatementExtractionStep(Nil, extraction(steps(sc), Nil))
  }
  def existingStatementExtraction(
    premises: SubstitutionContext => Seq[DerivedStatement],
    steps: SubstitutionContext => Seq[Step.AssertionStep])(
    implicit provingContext: ProvingContext
  ): SubstitutionContext => Step.ExistingStatementExtractionStep = { sc =>
    Step.ExistingStatementExtractionStep(premises(sc), extraction(steps(sc), Nil))
  }
  private def extraction(extractionSteps: Seq[AppliedExtractionStep], rewriteSteps: Seq[Step.AssertionStep])(implicit provingContext: ProvingContext): AppliedExtraction = {
    AppliedExtraction(extractionSteps, AppliedExtraction.pairChainedRewriteSteps(rewriteSteps), AppliedExtraction.endsInReversal(extractionSteps))
  }
  def inferenceExtraction(
    assertionStepConstructor: SubstitutionContext => Step.AssertionStep,
    extractionStepsConstructor: SubstitutionContext => Seq[Step.AssertionOrExtraction],
    rewriteStepsConstructor: SubstitutionContext => Seq[Step.AssertionStep] = (_ => Nil))(
    implicit provingContext: ProvingContext
  ): SubstitutionContext => Step.InferenceExtractionStep = { sc =>
    val assertionStep = assertionStepConstructor(sc)
    val extractionSteps = extractionStepsConstructor(sc).map(AppliedExtractionStep(_))
    val rewriteSteps = rewriteStepsConstructor(sc)
    Step.InferenceExtractionStep(AppliedInferenceExtraction(assertionStep, extraction(extractionSteps, rewriteSteps)))
  }
  def wrappedInferenceApplication(
    steps: SubstitutionContext => Seq[Step])(
    implicit provingContext: ProvingContext
  ): SubstitutionContext => Step.WrappedInferenceApplicationStep = { sc =>
    Step.WrappedInferenceApplicationStep(steps(sc))(provingContext)
  }
  def premiseDerivation(
    createPremises: SubstitutionContext => Seq[DerivedStatement],
    createAssertion: SubstitutionContext => Step.InferenceApplicationWithoutPremises
  ): SubstitutionContext => Step.InferenceWithPremiseDerivationsStep = { sc =>
    Step.InferenceWithPremiseDerivationsStep(
      createPremises(sc),
      createAssertion(sc))
  }
  def rewriteStep(
    createPremise: SubstitutionContext => DerivedStatement,
    createAssertion: SubstitutionContext => Step.AssertionStep
  ): SubstitutionContext => Step.RewriteStep = { sc =>
    Step.RewriteStep(RewritePremise.Known(createPremise(sc)), createAssertion(sc))
  }
  def rewriteStep(
    createPremises: SubstitutionContext => Seq[DerivedStatement],
    createExtraction: SubstitutionContext => Step.AssertionOrExtraction,
    createAssertion: SubstitutionContext => Step.AssertionStep
  ): SubstitutionContext => Step.RewriteStep = { sc =>
    Step.RewriteStep(
      RewritePremise.ByInference(
        createPremises(sc),
        createExtraction(sc) match {
          case step: Step.AssertionStep =>
            AppliedInferenceExtraction(step, AppliedExtraction.empty)
          case step: Step.InferenceExtractionStep =>
            step.inferenceExtraction
        }),
      createAssertion(sc))
  }
  def knownPremise(statement: Statement): SubstitutionContext => DerivedStatement = { _ =>
    DerivedStatement.Known(statement)
  }
  def asserted(createExtraction: SubstitutionContext => Step.InferenceExtractionStep): SubstitutionContext => DerivedStatement = { sc =>
    DerivedStatement.Asserted(createExtraction(sc).inferenceExtraction)
  }
  def substituted(
    createSource: SubstitutionContext => DerivedStatement,
    createEquality: SubstitutionContext => DerivedStatement,
    createSubstitution: SubstitutionContext => Step.AssertionStep
  ): SubstitutionContext => DerivedStatement = { sc =>
    DerivedStatement.Substituted(createSource(sc), createEquality(sc), createSubstitution(sc))
  }
  /**
    * A reversal of a known statement, e.g. "b = a" from "a = b".
    */
  def reversed(
    createReversal: SubstitutionContext => Step.AssertionStep
  ): SubstitutionContext => DerivedStatement = { sc =>
    val reversal = createReversal(sc)
    DerivedStatement.Reversed(DerivedStatement.Known(reversal.premises.head.statement), reversal)
  }
  /**
    * A combination of known statements, e.g. "φ ∧ ψ" from "φ" and "ψ".
    */
  def combined(
    createCombination: SubstitutionContext => Step.AssertionStep
  ): SubstitutionContext => DerivedStatement = { sc =>
    val combination = createCombination(sc)
    DerivedStatement.Combined(combination.premises.map(p => DerivedStatement.Known(p.statement)), combination)
  }
  implicit def knownStatementToDerivedStatement(createKnownStatement: SubstitutionContext => KnownStatement): SubstitutionContext => DerivedStatement = { sc =>
    DerivedStatement.Unstructured.fromKnownStatement(createKnownStatement(sc))
  }
  def known(
    statement: Statement
  ): SubstitutionContext => KnownStatement = { sc =>
    KnownStatement(statement, SimpleDerivation.empty)
  }
  def known(
    createSteps: SubstitutionContext => Seq[Step.AssertionOrExtraction]
  ): SubstitutionContext => KnownStatement = { sc =>
    val steps = createSteps(sc)
    val derivation = SimpleDerivation(steps.map {
      case step: Step.AssertionStep => step
      case step: Step.InferenceExtractionStep => step.inferenceExtraction
    })
    KnownStatement(derivation.statement, derivation)
  }

  def fillerSteps(number: Int): SubstitutionContext => Seq[Step] = (0 until number).map(i => target(ForAll("x")(Equals($, add($, $)))))

  implicit class StepsConstructor(createSteps: SubstitutionContext => Seq[Step]) {
    def :+(other: SubstitutionContext => Step): SubstitutionContext => Seq[Step] = { sc =>
      createSteps(sc) :+ other(sc)
    }
  }

  implicit def seqConstructorToConstructorSeq[T](seq: Seq[SubstitutionContext => T]): SubstitutionContext => Seq[T] = { sc =>
    seq.map(_(sc))
  }
  implicit def knownStatementSeqToDerivedStatementSeq(seq: Seq[SubstitutionContext => KnownStatement]): SubstitutionContext => Seq[DerivedStatement] = { sc =>
    seq.map(createKnownStatement => DerivedStatement.Unstructured.fromKnownStatement(createKnownStatement(sc)))
  }
}
