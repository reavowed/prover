package net.prover.refactoring

import net.prover.{StepBuilderHelper, StepHelpers}
import net.prover.model.*
import net.prover.model.TestDefinitions.{a, b, *}
import net.prover.model.proof.{StepContext, SubstitutionContext}
import org.specs2.mutable.Specification

class RederivePremisesSpec extends Specification with StepBuilderHelper with StepHelpers {
  given availableEntries: AvailableEntries = defaultAvailableEntries
  given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0, c -> 0))

  "rederiving premises" should {
    "replace a rewrite premise with the derivation found now" in {
      // The premise was derived by reversing "a = b", but is now available directly
      given outerStepContext: StepContext = createBaseStepContext(Seq(Equals(a, b), Equals(b, a), φ(b)))
      val initialStep = recalculateReferences(rewriteStep(
        reversed(assertion(reverseEquality, Nil, Seq(a, b))),
        assertion(substitutionOfEquals, Seq(φ($)), Seq(b, a)))(SubstitutionContext.outsideProof))
      val expectedStep = recalculateReferences(
        assertion(substitutionOfEquals, Seq(φ($)), Seq(b, a))(SubstitutionContext.outsideProof))

      RederivePremises(createStepWithContext(initialStep)) must beEqualTo(expectedStep)
    }

    "leave a premise that can't be rederived" in {
      given outerStepContext: StepContext = createBaseStepContext(Seq(Equals(a, c), φ(b)))
      val initialStep = rewriteStep(
        reversed(assertion(reverseEquality, Nil, Seq(a, b))),
        assertion(substitutionOfEquals, Seq(φ($)), Seq(b, a)))(SubstitutionContext.outsideProof)

      RederivePremises(createStepWithContext(initialStep)) must beEqualTo(initialStep)
    }
  }
}
