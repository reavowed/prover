package net.prover.model.proof;

import net.prover.model.TestDefinitions._
import net.prover.model.proof.DisplayStepMatchers.referenceStep
import net.prover.theorems.{DisplayStep, GetDisplaySteps, RecalculateReferences}
import net.prover.{StepBuilderHelper, StepHelpers}
import org.specs2.mutable.Specification

class RewriteStepSpec extends Specification with StepHelpers with StepBuilderHelper {
  implicit val availableEntries = defaultAvailableEntries
  "rewrite step" should {
    "match references to steps" in {
      // Regression test for a bug where the structure of RewritePremise.ByInference.toProofSteps
      // did not line up with the step structure implied when parsing, leading to a ref mismatch
      implicit val outerStepContext = StepContext.withPremisesAndVariables(
        Seq(ElementOf(a, Integers), ElementOf(b, Integers)),
        getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0)))
      val step = RecalculateReferences(
        createStepWithContext(
          rewriteStep( // 0
            Seq(
              known(Seq(
                inferenceExtraction( // 0.0
                  assertion(IntegerAdditionDefinition.definitionInference, Nil, Nil), // 0.0.0
                  Seq(inferenceExtraction( // 0.0.1
                    assertion(BinaryOperationOn.deconstructionInference, Nil, Seq(IntegerAddition, Integers)), // 0.0.1.0
                    Seq(assertion(reverseEquality, Nil, Seq(BaseSet(IntegerAddition), Integers)))))), // 0.0.1.1
                assertion(substitutionOfEquals, Seq(ElementOf(a, $)), Seq(Integers, BaseSet(IntegerAddition))))), // 0.1
              known(Seq(
                inferenceExtraction( // 0.2
                  assertion(IntegerAdditionDefinition.definitionInference, Nil, Nil),
                  Seq(inferenceExtraction(
                    assertion(BinaryOperationOn.deconstructionInference, Nil, Seq(IntegerAddition, Integers)),
                    Seq(assertion(reverseEquality, Nil, Seq(BaseSet(IntegerAddition), Integers)))))),
                assertion(substitutionOfEquals, Seq(ElementOf(b, $)), Seq(Integers, BaseSet(IntegerAddition)))))),
            inferenceExtraction(
              assertion(integerAdditionIsCommutative, Nil, Nil),
              Seq(
                inferenceExtraction(
                  assertion(Commutative.deconstructionInference, Nil, Seq(IntegerAddition)),
                  Seq(
                    assertion(specification, Seq(Implication(ElementOf($, BaseSet(IntegerAddition)), ForAllIn("b", BaseSet(IntegerAddition))(Equals(addZ($.^, $), addZ($, $.^))))), Seq(a)),
                    assertion(modusPonens, Seq(ElementOf(a, BaseSet(IntegerAddition)), ForAllIn("b", BaseSet(IntegerAddition))(Equals(addZ(a, $), addZ($, a)))), Nil),
                    assertion(specification, Seq(Implication(ElementOf($, BaseSet(IntegerAddition)), Equals(addZ(a, $), addZ($, a)))), Seq(b)),
                    assertion(modusPonens, Seq(ElementOf(b, BaseSet(IntegerAddition)), Equals(addZ(a, b), addZ(b, a))), Nil))))),
            assertion(equivalenceOfSubstitutedEquals, Seq(φ($)), Seq(addZ(a, b), addZ(b, a)))
          )(SubstitutionContext.outsideProof))).get._1
      val displayStep = GetDisplaySteps(step, Seq(0))

      val rewritePremiseStep = displayStep.asInstanceOf[DisplayStep.ElidedInference]
        .substeps.head.asInstanceOf[DisplayStep.ElidedInference]
      val firstKnownExtraction = rewritePremiseStep.substeps(0).asInstanceOf[DisplayStep.ElidedInference]
      val firstKnownSubstitution = rewritePremiseStep.substeps(1).asInstanceOf[DisplayStep.Assertion]
      val firstKnownSubstitutionPremise = firstKnownSubstitution.premises.head.asInstanceOf[Premise.Given]

      firstKnownSubstitutionPremise must referenceStep(firstKnownExtraction)
    }
  }
}
