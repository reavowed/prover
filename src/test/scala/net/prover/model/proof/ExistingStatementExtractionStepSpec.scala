package net.prover.model.proof

import net.prover.model.AvailableEntries
import net.prover.model.TestDefinitions.*
import net.prover.model.proof.DisplayStepMatchers.referenceStep
import net.prover.proving.derivation.DerivedStatement
import net.prover.proving.extraction.{AppliedExtraction, AppliedInferenceExtraction}
import net.prover.theorems.{DisplayStep, GetDisplaySteps, RecalculateReferences}
import net.prover.{StepBuilderHelper, StepHelpers}
import org.specs2.mutable.Specification

class ExistingStatementExtractionStepSpec extends Specification with StepHelpers with StepBuilderHelper {
  given availableEntries: AvailableEntries = defaultAvailableEntries
  "existing statement extraction step" should {
    "match references to steps" in {
      given outerStepContext: StepContext = StepContext.withPremisesAndVariables(
        Seq(φ(a), ψ(a), ForAll("x")(Implication(Conjunction(φ($), ψ($)), χ($)))),
        getVariableDefinitions(Seq(φ -> 1, ψ -> 1, χ -> 1), Seq(a -> 0)))
      val step = RecalculateReferences(
        createStepWithContext(
          existingStatementExtraction(
            Seq(known(Seq(assertion(combineConjunction, Seq(φ(a), ψ(a)), Nil)))),
            Seq(
              assertion(specification, Seq(Implication(Conjunction(φ($), ψ($)), χ($))), Seq(a)),
              assertion(modusPonens, Seq(Conjunction(φ(a), ψ(a)), χ(a)), Nil))
          ).apply(SubstitutionContext.outsideProof))
      ).get._1
      val displayStep = GetDisplaySteps(step, Seq(0)).asInstanceOf[DisplayStep.ElidedWithDescription]

      val premiseCombinationStep = displayStep.substeps(0).asInstanceOf[DisplayStep.Assertion]
      val extractionGroupStep = displayStep.substeps(1).asInstanceOf[DisplayStep.ElidedWithDescription]
      val firstExtractionStep = extractionGroupStep.substeps(0).asInstanceOf[DisplayStep.Assertion]
      val secondExtractionStep = extractionGroupStep.substeps(1).asInstanceOf[DisplayStep.Assertion]

      secondExtractionStep.premises(0) must referenceStep(firstExtractionStep)
      secondExtractionStep.premises(1) must referenceStep(premiseCombinationStep)
    }

    given SubstitutionContext = SubstitutionContext.outsideProof
    def display(premises: Seq[DerivedStatement], extractionSteps: Seq[Step.AssertionStep]): DisplayStep = {
      GetDisplaySteps(Step.ExistingStatementExtractionStep(premises, AppliedExtraction.fromSimpleExtraction(extractionSteps)), Seq(0))
    }
    val repeatedSpecification = Seq(
      assertion(specification, Seq(ForAll("y")(χ($.^, $))), Seq(a))(summon),
      assertion(specification, Seq(χ(a, $)), Seq(b))(summon))
    val modusPonensStep = assertion(modusPonens, Seq(Equals(a, b), φ(a)), Nil)(summon)

    "be described by the inference of an extraction with a single inference" in {
      display(Nil, repeatedSpecification) must beLike[DisplayStep] {
        case DisplayStep.ElidedInference(_, inference, _, _) => inference must beEqualTo(specification.summary)
      }
    }

    "be described by the inference of its extraction if its premises only needed rewrites" in {
      display(Seq(reversed(assertion(reverseEquality, Nil, Seq(b, a)))(summon)), Seq(modusPonensStep)) must beLike[DisplayStep] {
        case DisplayStep.ElidedInference(_, inference, _, _) => inference must beEqualTo(modusPonens.summary)
      }
    }

    "not be described by an inference if its premises needed other inferences" in {
      val premise = DerivedStatement.Simplified(
        DerivedStatement.Known(ElementOf(a, Product(b, c))),
        AppliedInferenceExtraction(assertion(firstCoordinateOfElementOfCartesianProduct, Nil, Seq(a, b, c))(summon), AppliedExtraction.empty))
      display(Seq(premise), Seq(modusPonensStep)) must beLike[DisplayStep] {
        case DisplayStep.ElidedWithDescription(_, description, _, _) => description must beSome("Extraction from previous step")
      }
    }
  }
}
