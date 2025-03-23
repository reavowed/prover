package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model.{AvailableEntries, VariableDefinitions}
import net.prover.model.TestDefinitions.*

class StepCreationSpec extends ControllerSpec {

  given availableEntries: AvailableEntries = defaultAvailableEntries
  given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 0, χ -> 0), Nil)

  "adding a target" should {
    "insert the new step before any transitivity" in {
      given service: BookService = mock[BookService]
      mockReplaceStepsForInsertion(service)

      val controller = new StepCreationController

      controller.addTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        φ.toVariable.serialized)

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 1) :+ target(Equivalence(φ, ψ)) :+ target(Equivalence(ψ, χ)) :+ assertion(equivalenceIsTransitive, Seq(φ, ψ, χ), Nil),
        fillerSteps(stepIndex - 1) :+ target(φ):+ target(Equivalence(φ, ψ)) :+ target(Equivalence(ψ, χ)) :+ assertion(equivalenceIsTransitive, Seq(φ, ψ, χ), Nil))
    }
  }

}
