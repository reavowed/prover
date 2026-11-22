package net.prover.refactoring

import net.prover.{StepBuilderHelper, StepHelpers}
import net.prover.model.*
import net.prover.model.TestDefinitions.{a, b, *}
import net.prover.model.proof.{Step, StepContext, SubstitutionContext}
import org.specs2.mutable.Specification

class RederivePremisesSpec extends Specification with StepBuilderHelper with StepHelpers {
  given availableEntries: AvailableEntries = defaultAvailableEntries
  given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0, c -> 0))

  "rederiving premises" should {
    "replace a legacy rewrite premise with a structured one" in {
      given outerStepContext: StepContext = createBaseStepContext(Seq(Equals(a, b), φ(b)))
      val legacyStep = recalculateReferences(rewriteStep(
        known(Seq(assertion(reverseEquality, Nil, Seq(a, b)))),
        assertion(substitutionOfEquals, Seq(φ($)), Seq(b, a)))(SubstitutionContext.outsideProof))
      val expectedStep = recalculateReferences(rewriteStep(
        reversed(assertion(reverseEquality, Nil, Seq(a, b))),
        assertion(substitutionOfEquals, Seq(φ($)), Seq(b, a)))(SubstitutionContext.outsideProof))

      RederivePremises(createStepWithContext(legacyStep)) must beEqualTo(expectedStep)
    }

    "leave a legacy premise that can't be rederived" in {
      given outerStepContext: StepContext = createBaseStepContext(Seq(Equals(a, c), φ(b)))
      val legacyStep = rewriteStep(
        known(Seq(assertion(reverseEquality, Nil, Seq(a, b)))),
        assertion(substitutionOfEquals, Seq(φ($)), Seq(b, a)))(SubstitutionContext.outsideProof)

      RederivePremises(createStepWithContext(legacyStep)) must beEqualTo(legacyStep)
    }
  }
}
