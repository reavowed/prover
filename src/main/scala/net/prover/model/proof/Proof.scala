package net.prover.model.proof

import net.prover.model.expressions.{Statement, TermVariable}
import net.prover.model._
import net.prover.model.entries.StatementDefinition
import org.slf4j.LoggerFactory

case class Proof(steps: Seq[Step]) {
  def referencedInferenceIds: Set[String] = steps.flatMap(_.referencedInferenceIds).toSet
  def referenceMap: ReferenceMap = steps.map(_.referenceMap).foldTogether
  val conclusion: Statement = {
    steps.ofType[Step.WithAssertion].lastOption
      .flatMap(_.assertion.asOptionalInstanceOf[Statement])
      .getOrElse(throw new Exception("Proof must contain at least one top-level proven statement"))
  }
}

object Proof {
  val logger = LoggerFactory.getLogger(Proof.getClass)

  def getInitialContext(
    premises: Seq[Premise],
    availableInferences: Seq[Inference],
    assertionHints: Seq[AssertionHint],
    transformations: Seq[StatementDefinition]
  ): ProvingContext = {
    ProvingContext(
      premises.map(_.referencedFact),
      premises,
      Nil,
      availableInferences,
      assertionHints,
      transformations,
      0)
  }

  def fillInOutline(
    premises: Seq[Premise],
    proofOutline: ProofOutline,
    availableInferences: Seq[Inference],
    assertionHints: Seq[AssertionHint],
    transformations: Seq[StatementDefinition]
  ): Proof = {
    val context = getInitialContext(premises, availableInferences, assertionHints, transformations)
    val detailedSteps = proveSteps(
      proofOutline.steps,
      Nil,
      context,
      None)
    Proof(detailedSteps)
  }

  private def proveSteps(
    stepOutlines: Seq[StepOutline],
    accumulatedSteps: Seq[Step],
    context: ProvingContext,
    baseReference: Option[Reference.Direct]
  ): Seq[Step] = {
    stepOutlines.zipWithIndex.foldLeft(Seq.empty[Step]) { case (steps, (stepOutline, index)) =>
      val updatedContext = context.copy(referencedFacts = context.referencedFacts ++ steps.mapCollect(_.referencedFact))
      steps :+ proveStep(stepOutline, updatedContext, Reference.nextReference(baseReference, index.toString))
    }
  }

  private def proveStep(
    stepOutline: StepOutline,
    context: ProvingContext,
    reference: Reference.Direct
  ): Step = {
    stepOutline match {
      case StepOutline.Assumption(assumption, substepOutlines) =>
        proveAssumptionStep(assumption, substepOutlines, context, reference)
      case StepOutline.Naming(variable, namingStatement, substepOutlines) =>
        proveNamingStep(variable, namingStatement, substepOutlines, context, reference)
      case assertionStep: StepOutline.Assertion =>
        proveAssertionStep(assertionStep, context, reference)
          .getOrElse(throw ProvingException(
            s"Could not prove assertion ${assertionStep.assertion}",
            assertionStep.location))
      case StepOutline.ScopedVariable(boundVariableName, substepOutlines) =>
        proveScopedVariableStep(boundVariableName, substepOutlines, context, reference)
    }
  }

  private def proveAssumptionStep(
    assumption: Statement,
    substepOutlines: Seq[StepOutline],
    context: ProvingContext,
    reference: Reference.Direct
  ): Step.Assumption = {
    val contextWithAssumption = context.addFact(Fact.Direct(assumption), reference.withSuffix("a"))
    val substeps = proveSteps(
      substepOutlines,
      Nil,
      contextWithAssumption,
      Some(reference))
    Step.Assumption(assumption, substeps, reference)
  }

  private def proveAssertionStep(
    stepOutline: StepOutline.Assertion,
    context: ProvingContext,
    reference: Reference.Direct
  ): Option[Step.Assertion] = {
    Prover(
      stepOutline.assertion,
      reference,
      context,
      stepOutline.debug
    ).proveAssertion()
  }

  def proveNamingStep(
    variable: TermVariable,
    definingAssumption: Statement,
    substepOutlines: Seq[StepOutline],
    context: ProvingContext,
    reference: Reference.Direct
  ): Step.Naming = {
    val finalStepWithAssertion = substepOutlines match {
      case _ :+ (step: StepOutline.WithAssertion) =>
        step.innermostAssertionStep
      case _ =>
        throw new Exception("Naming step must end with an assertion")
    }
    val assumptionStep = proveAssumptionStep(definingAssumption, substepOutlines, context, reference)
    val deduction = assumptionStep.referencedFact.getOrElse(throw ProvingException(
      "Naming step did not have a conclusion",
      finalStepWithAssertion.location))
    val assertionStep = proveAssertionStep(
      finalStepWithAssertion,
      context.addFact(deduction.fact, deduction.reference.asInstanceOf[Reference.Direct].withSuffix("d")),
      reference
    ).getOrElse(
      throw ProvingException(
        s"Could not extract assertion ${finalStepWithAssertion.assertion} from naming step for $variable",
        finalStepWithAssertion.location))
    Step.Naming(variable, assumptionStep, assertionStep, reference)
  }

  def proveScopedVariableStep(
    boundVariableName: String,
    substepOutlines: Seq[StepOutline],
    context: ProvingContext,
    reference: Reference.Direct
  ): Step.ScopedVariable = {
    val substeps = proveSteps(substepOutlines, Nil, context.increaseDepth(1, context.depth), Some(reference))
    Step.ScopedVariable(boundVariableName, substeps, reference)
  }
}
