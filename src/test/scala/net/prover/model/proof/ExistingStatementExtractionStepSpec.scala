package net.prover.model.proof

import net.prover.model.TestDefinitions._
import net.prover.model.proof.DisplayStepMatchers.referenceStep
import net.prover.theorems.{DisplayStep, GetDisplaySteps, RecalculateReferences}
import net.prover.{StepBuilderHelper, StepHelpers}
import org.specs2.mutable.Specification

class ExistingStatementExtractionStepSpec extends Specification with StepHelpers with StepBuilderHelper {
  implicit val availableEntries = defaultAvailableEntries
  "existing statement extraction step" should {
    "match references to steps" in {
      implicit val outerStepContext = StepContext.withPremisesAndVariables(
        Seq(φ(a), ψ(a), ForAll("x")(Implication(Conjunction(φ($), ψ($)), χ($)))),
        getVariableDefinitions(Seq(φ -> 1, ψ -> 1, χ -> 1), Seq(a -> 0)))
      val step = RecalculateReferences(
        createStepWithContext(
          existingStatementExtraction(
            Seq(known(Seq(assertion(combineConjunction, Seq(φ(a), ψ(a)), Nil)))),
            Seq(
              assertion(specification, Seq(Implication(Conjunction(φ($), ψ($)), χ($))), Seq(a)),
              assertion(modusPonens, Seq(Conjunction(φ(a), ψ(a)), χ(a)), Nil))
          )(SubstitutionContext.outsideProof))
      ).get._1
      val displayStep = GetDisplaySteps(step, Seq(0)).asInstanceOf[DisplayStep.ElidedWithDescription]

      val premiseCombinationStep = displayStep.substeps(0).asInstanceOf[DisplayStep.Assertion]
      val extractionGroupStep = displayStep.substeps(1).asInstanceOf[DisplayStep.ElidedWithDescription]
      val firstExtractionStep = extractionGroupStep.substeps(0).asInstanceOf[DisplayStep.Assertion]
      val secondExtractionStep = extractionGroupStep.substeps(1).asInstanceOf[DisplayStep.Assertion]

      secondExtractionStep.premises(0) must referenceStep(firstExtractionStep)
      secondExtractionStep.premises(1) must referenceStep(premiseCombinationStep)
    }
  }
}
