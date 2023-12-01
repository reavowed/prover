package net.prover.refactoring

import net.prover.StepBuilderHelper
import net.prover.model.TestDefinitions._
import net.prover.model.proof.SubstitutionContext
import org.specs2.mutable.Specification

class ReplaceElidedStepsSpec extends Specification with StepBuilderHelper {

  implicit val availableEntries = defaultAvailableEntries

  "replace elided steps" should {
    "replace a wrapped assertion" in {
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 0), Nil)
      implicit val outerStepContext = createOuterStepContext(Nil)

      val initialSteps = recalculateReferences(Seq(
        target(ForAll("x")(Implication(φ($), ψ))),
        target(Negation(ψ)),
        elided(modusTollens, Seq(
          generalization("x", Seq(
            assertion(specification, Seq(Implication(φ($.^), ψ)), Seq($)),
            assertion(modusTollens, Seq(φ($), ψ), Nil)))))
      )(SubstitutionContext.outsideProof))
      val expectedSteps = recalculateReferences(Seq(
        target(ForAll("x")(Implication(φ($), ψ))),
        target(Negation(ψ)),
        wrappedInferenceApplication(Seq(
          generalization("x", Seq(
            assertion(specification, Seq(Implication(φ($.^), ψ)), Seq($)),
            assertion(modusTollens, Seq(φ($), ψ), Nil)))))
      )(SubstitutionContext.outsideProof))

      ReplaceElidedSteps(createStepsWithContext(initialSteps)) mustEqual expectedSteps
    }

    "replace an extraction" in {
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(a -> 0, b -> 0))
      implicit val outerStepContext = createOuterStepContext(Nil)

      val initialSteps = recalculateReferences(Seq(
        target(Equals(a, b)),
        elided(membershipConditionForSingleton, Seq(
          assertion(membershipConditionForSingleton, Nil, Seq(b)),
          assertion(specification, Seq(Equivalence(ElementOf($, Singleton(b)), Equals($, b))), Seq(a)),
          assertion(reverseImplicationFromEquivalence, Seq(ElementOf(a, Singleton(b)), Equals(a, b)), Nil),
          assertion(modusPonens, Seq(Equals(a, b), ElementOf(a, Singleton(b))), Nil)))
      )(SubstitutionContext.outsideProof))
      val expectedSteps = recalculateReferences(Seq(
        target(Equals(a, b)),
        inferenceExtraction(Seq(
          assertion(membershipConditionForSingleton, Nil, Seq(b)),
          assertion(specification, Seq(Equivalence(ElementOf($, Singleton(b)), Equals($, b))), Seq(a)),
          assertion(reverseImplicationFromEquivalence, Seq(ElementOf(a, Singleton(b)), Equals(a, b)), Nil),
          assertion(modusPonens, Seq(Equals(a, b), ElementOf(a, Singleton(b))), Nil)))
      )(SubstitutionContext.outsideProof))

      ReplaceElidedSteps(createStepsWithContext(initialSteps)) mustEqual expectedSteps
    }
  }

}
