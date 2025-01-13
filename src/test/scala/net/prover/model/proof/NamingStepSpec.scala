package net.prover.model.proof

import net.prover.model.{Substitutions, VariableDefinitions}
import net.prover.model.TestDefinitions._
import org.specs2.mutable.Specification

class NamingStepSpec extends Specification {
  "a naming step" should {
    "correctly specify a step context" in {
      val step = Step.NamingStep(
        "y",
        φ($),
        ψ,
        Seq(Step.TargetStep(ψ)),
        valueForExistence.summary,
        Nil,
        Substitutions(Seq(φ($), ψ), Nil),
        GeneralizationDefinition,
        DeductionDefinition)
      val outerStepContext = StepContext.withPremisesAndVariables(Nil, VariableDefinitions.empty)
        .addBoundVariable("x")
        .addAssumption(χ($))

      val innerStepContext = step.specifyStepContext(outerStepContext)

      innerStepContext.premises must contain(exactly(
        beEqualTo(χ($.^)) ^^ { (_: Premise).statement },
        beEqualTo(φ($)) ^^ { (_: Premise).statement }))
    }
  }
}
