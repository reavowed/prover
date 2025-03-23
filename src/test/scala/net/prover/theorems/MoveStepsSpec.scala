package net.prover.theorems

import net.prover.controllers.BookService
import net.prover.controllers.models.StepMoveRequest
import net.prover.model.TestDefinitions.*
import net.prover.model.{AvailableEntries, VariableDefinitions}
import net.prover.{BookServiceHelper, StepHelpers}
import org.specs2.mutable.Specification

class MoveStepsSpec extends Specification with BookServiceHelper with StepHelpers {
  given availableEntries: AvailableEntries = defaultAvailableEntries
  given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 0, χ -> 0), Seq(a -> 0))

  "move steps" should {
    "correctly move a step referencing a bound variable inside a generalization" in {
      given service: BookService = mock[BookService]
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
      given service: BookService = mock[BookService]
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
