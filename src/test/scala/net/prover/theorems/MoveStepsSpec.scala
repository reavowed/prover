package net.prover.theorems

import net.prover.BookServiceHelper
import net.prover.controllers.BookService
import net.prover.controllers.models.StepMoveRequest
import net.prover.model.TestDefinitions._
import org.specs2.mutable.Specification

class MoveStepsSpec extends Specification with BookServiceHelper {
  implicit val availableEntries = defaultAvailableEntries
  implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 0, χ -> 0), Seq(a -> 0))

  "move steps" should {
    "correctly move a step referencing a bound variable inside a generalization" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)

      MoveSteps(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        StepMoveRequest(outerStepPath, 0, 1, outerStepPath :+ 0, 0))

      checkModifySteps(
        service,
        Seq(
          target(Equals($, a)),
          generalization("y", Seq(
            target(φ)))),
        Seq(
          generalization("y", Seq(
            target(Equals($.^, a)),
            target(φ)))),
        Seq("x"))
    }

    "correctly move a step referencing a bound variable out of a generalization" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)

      MoveSteps(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        StepMoveRequest(outerStepPath :+ 0, 0, 1, outerStepPath, 0))

      checkModifySteps(
        service,
        Seq(
          generalization("y", Seq(
            target(Equals($.^, a)),
            target(φ)))),
        Seq(
          target(Equals($, a)),
          generalization("y", Seq(
            target(φ)))),
        Seq("x"))
    }
  }
}
