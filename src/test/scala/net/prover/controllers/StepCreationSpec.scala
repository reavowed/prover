package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model.TestDefinitions._

class StepCreationSpec extends ControllerSpec {

  implicit val entryContext = defaultEntryContext
  implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 0, χ -> 0), Nil)

  "adding a target" should {
    "insert the new step before any transitivity" in {
      implicit val service = mock[BookService]
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
