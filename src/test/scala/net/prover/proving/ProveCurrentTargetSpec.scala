package net.prover.proving

import net.prover.controllers.BookService
import net.prover.controllers.models.{PathData, SerializedSubstitutions, StepDefinition}
import net.prover.model.TestDefinitions._
import net.prover.{BookServiceHelper, StepContextHelper}
import org.specs2.mutable.Specification

class ProveCurrentTargetSpec extends Specification with BookServiceHelper {
  implicit val entryContext = defaultEntryContext
  implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 0, χ -> 0), Nil)

  "prove current target" should {
    "replace target with assertion step" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)

      ProveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        StepDefinition(
          Some(modusPonens.id),
          None,
          SerializedSubstitutions(Seq(φ.serialized, ψ.serialized), Nil),
          Nil,
          Nil,
          None,
          None,
          None))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 2) :+
          target(Implication(φ, ψ)) :+
          target(φ) :+
          target(ψ),
        fillerSteps(stepIndex - 2) :+
          target(Implication(φ, ψ)) :+
          target(φ) :+
          assertion(
            modusPonens,
            Seq(φ, ψ),
            Nil))
    }

    "add premise finding steps" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)

      ProveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        StepDefinition(
          Some(modusPonens.id),
          None,
          SerializedSubstitutions(Seq(Conjunction(φ, ψ).serialized, χ.serialized), Nil),
          Nil,
          Nil,
          None,
          None,
          None))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 3) :+
          target(Implication(Conjunction(φ, ψ), χ)) :+
          target(φ) :+
          target(ψ) :+
          target(χ),
        fillerSteps(stepIndex - 3) :+
          target(Implication(Conjunction(φ, ψ), χ)) :+
          target(φ) :+
          target(ψ) :+
          elided(
            modusPonens,
            Seq(
              assertion(
                combineConjunction,
                Seq(φ, ψ),
                Nil),
              assertion(
                modusPonens,
                Seq(Conjunction(φ, ψ), χ),
                Nil))))
    }
  }
}
