package net.prover

import net.prover.model.{Inference, ProvingContext, Substitutions}
import net.prover.model.expressions.{Statement, Term, TermVariable}
import net.prover.model.proof.{Step, SubstitutionContext}
import net.prover.model.TestDefinitions._
import net.prover.model.definitions.KnownStatement
import net.prover.model.proof.Step.RewriteStep
import net.prover.proving.derivation.{SimpleDerivation, SimpleDerivationStep}
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
    steps: SubstitutionContext => Seq[Step.AssertionStep]
  ): SubstitutionContext => Step.ExistingStatementExtractionStep = { sc =>
    Step.ExistingStatementExtractionStep(Nil, AppliedExtraction(steps(sc).map(AppliedExtractionStep.Assertion(_)), Nil))
  }
  def existingStatementExtraction(
    premises: SubstitutionContext => Seq[KnownStatement],
    steps: SubstitutionContext => Seq[Step.AssertionStep]
  ): SubstitutionContext => Step.ExistingStatementExtractionStep = { sc =>
    Step.ExistingStatementExtractionStep(premises(sc), AppliedExtraction(steps(sc).map(AppliedExtractionStep.Assertion(_)), Nil))
  }
  def inferenceExtraction(
    assertionStepConstructor: SubstitutionContext => Step.AssertionStep,
    extractionStepsConstructor: SubstitutionContext => Seq[Step.AssertionOrExtraction],
    rewriteStepsConstructor: SubstitutionContext => Seq[Step.AssertionStep] = (_ => Nil)
  ): SubstitutionContext => Step.InferenceExtractionStep = { sc =>
    val assertionStep = assertionStepConstructor(sc)
    val extractionSteps = extractionStepsConstructor(sc).map(AppliedExtractionStep(_))
    val rewriteSteps = rewriteStepsConstructor(sc)
    Step.InferenceExtractionStep(AppliedInferenceExtraction(assertionStep, AppliedExtraction(extractionSteps, rewriteSteps)))
  }
  def wrappedInferenceApplication(
    steps: SubstitutionContext => Seq[Step])(
    implicit provingContext: ProvingContext
  ): SubstitutionContext => Step.WrappedInferenceApplicationStep = { sc =>
    Step.WrappedInferenceApplicationStep(steps(sc))(provingContext)
  }
  def premiseDerivation(
    createPremises: SubstitutionContext => Seq[KnownStatement],
    createAssertion: SubstitutionContext => Step.InferenceApplicationWithoutPremises
  ): SubstitutionContext => Step.InferenceWithPremiseDerivationsStep = { sc =>
    Step.InferenceWithPremiseDerivationsStep(
      createPremises(sc),
      createAssertion(sc))
  }
  def rewriteStep(
    createPremise: SubstitutionContext => KnownStatement,
    createAssertion: SubstitutionContext => Step.AssertionStep
  ): SubstitutionContext => Step.RewriteStep = { sc =>
    Step.RewriteStep(RewritePremise.Known(createPremise(sc)), createAssertion(sc))
  }
  def rewriteStep(
    createPremises: SubstitutionContext => Seq[KnownStatement],
    createExtraction: SubstitutionContext => Step.AssertionOrExtraction,
    createAssertion: SubstitutionContext => Step.AssertionStep
  ): SubstitutionContext => Step.RewriteStep = { sc =>
    Step.RewriteStep(
      RewritePremise.ByInference(
        createPremises(sc),
        createExtraction(sc) match {
          case step: Step.AssertionStep =>
            AppliedInferenceExtraction(step, AppliedExtraction(Nil, Nil))
          case step: Step.InferenceExtractionStep =>
            step.inferenceExtraction
        }),
      createAssertion(sc))
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
      case step: Step.AssertionStep => SimpleDerivationStep.Assertion(step)
      case step: Step.InferenceExtractionStep => SimpleDerivationStep.InferenceExtraction(step.inferenceExtraction)
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
}
