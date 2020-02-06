package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model.TestDefinitions._

class StepProvingSpec extends ControllerSpec {
  "proving a step" should {
    "remove unnecessary structural simplifications" in {
      val service = createService
      val controller = new StepProvingController(service)

      val premise = Conjunction(φ, Implication(ψ, χ))

      controller.proveCurrentTarget(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), definition(premise, Map.empty, Seq(extractRightConjunct, modusPonens)))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(ψ) :+ target(χ),
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(ψ) :+ assertion(modusPonens, Seq(ψ, χ), Nil))
    }
    "not remove necessary structural simplifications" in {
      val service = createService
      val controller = new StepProvingController(service)

      val premise = Implication(φ, Conjunction(ψ, χ))

      controller.proveCurrentTarget(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), definition(premise, Map.empty, Seq(modusPonens, extractRightConjunct)))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(φ) :+ target(χ),
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(φ) :+ elided("Extracted", Seq(
          assertion(modusPonens, Seq(φ, Conjunction(ψ, χ)), Nil),
          assertion(extractRightConjunct, Seq(ψ, χ), Nil))))
    }
  }
}
