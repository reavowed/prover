package net.prover.proving.fromExistingStatement

import net.prover.ContextHelper
import net.prover.controllers.BookService
import net.prover.controllers.models.{PathData, PossibleConclusionWithPremises}
import net.prover.model.TestDefinitions._
import net.prover.model.proof.Step
import org.specs2.mutable.Specification

import scala.util.Success

class SuggestExistingStatementsForNewTargetSpec extends Specification with ContextHelper with PossibleConclusionMatchers {
  implicit val availableEntries = defaultAvailableEntries
  implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 1, χ -> 0), Seq(a -> 0))


  "SuggestExistingStatementsForNewTarget" should {
    "suggest all extractions from an existing statement" in {
      val premise = ForAll("x")(Implication(φ($), ψ($)))
      val target = χ
      implicit val service = mock[BookService]
      implicit val stepContext = createBaseStepContext(Seq(premise))
      service.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath)) returns
        Success(createTargetStepWithContext(target))

      val result = SuggestExistingStatementsForNewTarget(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), premise.serialized)

      result must beSuccessfulTry(contain[PossibleConclusionWithPremises](exactly(
        bePossibleConclusionWithPremises(ForAll("x")(Implication(φ($), ψ($))), Nil),
        bePossibleConclusionWithPremises(Implication(φ(b), ψ(b)), Nil),
        bePossibleConclusionWithPremises(ψ(b), Seq(φ(b))),
        bePossibleConclusionWithPremises(Negation(φ(b)), Seq(Negation(ψ(b))))
      )))
    }
  }
}
