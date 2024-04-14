package net.prover

import net.prover.model.{Inference, ProvingContext, Substitutions}
import net.prover.model.expressions.{Statement, Term, TermVariable}
import net.prover.model.proof.{Step, SubstitutionContext}
import net.prover.model.TestDefinitions._

import scala.language.implicitConversions

trait StepHelpers {
  def assertion(inference: Inference, statements: Seq[Statement], terms: Seq[Term]): SubstitutionContext => Step.AssertionStep = { substitutionContext =>
    Step.AssertionStep.forInference(inference, Substitutions(statements, terms))(substitutionContext).get
  }

  def generalization(variableName: String, steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.GeneralizationStep = sc => Step.GeneralizationStep(variableName, steps(SubstitutionContext.withExtraParameter(sc)), GeneralizationDefinition)
  def deduction(antecedent: Statement, steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.DeductionStep = sc => Step.DeductionStep(antecedent, steps(sc), DeductionDefinition)
  def target(statement: Statement): SubstitutionContext => Step.TargetStep = _ => Step.TargetStep(statement)
  def elided(inference: Inference, steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.ElidedStep = sc => Step.ElidedStep(steps(sc), Some(inference.summary), None)
  def elided(description: String, steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.ElidedStep = sc => Step.ElidedStep(steps(sc), None, Some(description))
  def existingStatementExtraction(steps: SubstitutionContext => Seq[Step.AssertionStep]): SubstitutionContext => Step.ExistingStatementExtractionStep = sc => Step.ExistingStatementExtractionStep(steps(sc))
  def inferenceExtraction(steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.InferenceExtractionStep = sc => Step.InferenceExtractionStep(steps(sc))
  def wrappedInferenceApplication(steps: SubstitutionContext => Seq[Step])(implicit provingContext: ProvingContext): SubstitutionContext => Step.WrappedInferenceApplicationStep = sc => Step.WrappedInferenceApplicationStep(steps(sc))(provingContext)
  def premiseDerivation(steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.InferenceWithPremiseDerivationsStep = sc => Step.InferenceWithPremiseDerivationsStep(steps(sc))

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
