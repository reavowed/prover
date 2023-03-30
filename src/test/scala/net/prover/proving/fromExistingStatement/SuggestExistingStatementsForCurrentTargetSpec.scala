package net.prover.proving.fromExistingStatement

import net.prover.StepContextHelper
import net.prover.controllers.BookService
import net.prover.controllers.models.{PathData, PossibleConclusionWithPremises}
import net.prover.model.TestDefinitions._
import net.prover.model.proof.{Step, StepProvingContext}
import org.specs2.mutable.Specification

import scala.util.Success

class SuggestExistingStatementsForCurrentTargetSpec extends Specification with StepContextHelper with PossibleConclusionMatchers {
  implicit val availableEntries = defaultAvailableEntries
  implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 1, χ -> 0), Seq(a -> 0))

  "SuggestExistingStatementsForCurrentTarget" should {
    "suggest all extractions from an existing statement" in {
      val premise = ForAll("x")(Implication(φ($), ψ($)))
      val target = Negation(φ(a))
      implicit val service = mock[BookService]
      implicit val stepContext = createBaseStepContext(Seq(premise))
      service.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath)) returns
        Success(createTargetStepWithContext(target))

      val result = SuggestExistingStatementsForCurrentTarget(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), premise.serialized)

      result must beSuccessfulTry(contain[PossibleConclusionWithPremises](exactly(
        bePossibleConclusionWithPremises(Negation(φ(b)), Seq(Negation(ψ(b))))
      )))
    }
  }
}
