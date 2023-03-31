package net.prover.theorems

import net.prover.StepBuilderHelper
import net.prover.model.TestDefinitions._
import net.prover.model.proof.SubstitutionContext
import org.specs2.mutable.Specification

class FindStepsSpec extends Specification with StepBuilderHelper {
  implicit val availableEntries = defaultAvailableEntries
  implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 0), Nil)

  "find steps" should {
    "find a target step" in {
      val theorem = createTheorem(
        Nil,
        Seq(
          target(Implication(φ, ψ)),
          target(φ),
          target(ψ)))

      FindStep(createTheoremWithContext(theorem), 0, Seq(1)).map(_.step) must beSome(target(φ)(SubstitutionContext.outsideProof))
    }
  }
}
