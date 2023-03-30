package net.prover.proving

import net.prover.BookServiceHelper
import net.prover.controllers.BookService
import net.prover.controllers.models.{PathData, SerializedSubstitutions, StepDefinition}
import net.prover.model.TestDefinitions._
import org.specs2.mutable.Specification

class ProveCurrentTargetSpec extends Specification with BookServiceHelper {
  implicit val availableEntries = defaultAvailableEntries
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

    "add targets that don't exist" in {
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
        fillerSteps(stepIndex - 1) :+
          target(Implication(φ, ψ)) :+
          target(ψ),
        fillerSteps(stepIndex - 1) :+
          target(Implication(φ, ψ)) :+
          target(φ) :+
          assertion(
            modusPonens,
            Seq(φ, ψ),
            Nil))

    }

    "add targets before chain" in {
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
          SerializedSubstitutions(Seq(φ.serialized, Implication(ψ, χ).serialized), Nil),
          Nil,
          Nil,
          None,
          None,
          None))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 2) :+
          target(Implication(φ, Implication(ψ, χ))) :+
          target(Implication(φ, ψ)) :+
          target(Implication(ψ, χ)) :+
          assertion(implicationIsTransitive, Seq(φ, ψ, χ), Nil),
        fillerSteps(stepIndex - 2) :+
          target(Implication(φ, Implication(ψ, χ))) :+
          target(φ) :+
          target(Implication(φ, ψ)) :+
          assertion(modusPonens, Seq(φ, Implication(ψ, χ)), Nil) :+
          assertion(implicationIsTransitive, Seq(φ, ψ, χ), Nil))
    }
  }
}
