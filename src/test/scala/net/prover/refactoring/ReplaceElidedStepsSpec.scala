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
  }

}
