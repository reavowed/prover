package net.prover

import net.prover.model.{Inference, ProvingContext, Substitutions}
import net.prover.model.expressions.{Statement, Term, TermVariable}
import net.prover.model.proof.{Step, SubstitutionContext}
import net.prover.model.TestDefinitions._

import scala.language.implicitConversions

trait StepHelpers {
  def assertion(inference: Inference, statements: Seq[Statement], terms: Seq[Term]): SubstitutionContext => Step.Assertion = { substitutionContext =>
    Step.Assertion.forInference(inference, Substitutions(statements, terms))(substitutionContext).get
  }

  def generalization(variableName: String, steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.Generalization = sc => Step.Generalization(variableName, steps(SubstitutionContext.withExtraParameter(sc)), GeneralizationDefinition)
  def deduction(antecedent: Statement, steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.Deduction = sc => Step.Deduction(antecedent, steps(sc), DeductionDefinition)
  def target(statement: Statement): SubstitutionContext => Step.Target = _ => Step.Target(statement)
  def elided(inference: Inference, steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.Elided = sc => Step.Elided(steps(sc), Some(inference.summary), None)
  def elided(description: String, steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.Elided = sc => Step.Elided(steps(sc), None, Some(description))
  def existingStatementExtraction(steps: SubstitutionContext => Seq[Step.Assertion]): SubstitutionContext => Step.ExistingStatementExtraction = sc => Step.ExistingStatementExtraction(steps(sc))
  def inferenceExtraction(steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.InferenceExtraction = sc => Step.InferenceExtraction(steps(sc))
  def premiseDerivation(steps: SubstitutionContext => Seq[Step])(implicit provingContext: ProvingContext): SubstitutionContext => Step.InferenceWithPremiseDerivations = sc => Step.InferenceWithPremiseDerivations(steps(sc))(provingContext)

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
