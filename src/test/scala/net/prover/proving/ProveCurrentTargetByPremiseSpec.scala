package net.prover.proving

import net.prover.{BookServiceHelper, StepHelpers}
import net.prover.controllers.BookService
import net.prover.controllers.models.{PathData, SerializedSubstitutions, StepDefinition}
import net.prover.model.TestDefinitions._
import net.prover.proving.extraction.ExtractionDefinition
import org.specs2.mutable.Specification

class ProveCurrentTargetByPremiseSpec extends Specification with BookServiceHelper with StepHelpers {
  "prove current target by premise" should {
    "prove a simple target" in {
      implicit val availableEntries = defaultAvailableEntries
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 1), Seq(a -> 0))

      implicit val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)

      val mainPremise = ForAll("x")(Implication(φ($), ψ($)))
      val subsidiaryPremise = φ(a)
      val conclusion = ψ(a)

      ProveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        StepDefinition(
          None,
          Some(mainPremise.serialized),
          SerializedSubstitutions(Seq(φ($).serialized, ψ($).serialized), Seq(a.serialized, a.serialized)),
          ExtractionDefinition(
            Seq(specification.summary, modusPonens.summary)
          ).serialized,
          Nil,
          Some(Seq(subsidiaryPremise.serialized)),
          Some(conclusion.serialized),
          None))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 2) :+
          target(mainPremise) :+
          target(subsidiaryPremise) :+
          target(conclusion),
        fillerSteps(stepIndex - 2) :+
          target(mainPremise) :+
          target(subsidiaryPremise) :+
          existingStatementExtraction(Seq(
            assertion(specification, Seq(Implication(φ($), ψ($))), Seq(a)),
            assertion(modusPonens, Seq(φ(a), ψ(a)), Nil))))
    }

    "prove with a simple premise derivation" in {
      implicit val availableEntries = defaultAvailableEntries
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 1, χ -> 1), Seq(a -> 0))

      implicit val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)

      val mainPremise = ForAll("x")(Implication(Conjunction(φ($), ψ($)), χ($)))
      val subsidiaryPremise = Conjunction(φ(a), ψ(a))
      val conclusion = χ(a)

      ProveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        StepDefinition(
          None,
          Some(mainPremise.serialized),
          SerializedSubstitutions(Seq(φ($).serialized, ψ($).serialized, χ($).serialized), Seq(a.serialized, a.serialized)),
          ExtractionDefinition(
            Seq(specification.summary, modusPonens.summary)
          ).serialized,
          Nil,
          Some(Seq(subsidiaryPremise.serialized)),
          Some(conclusion.serialized),
          None))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 3) :+
          target(mainPremise) :+
          target(φ(a)) :+
          target(ψ(a)) :+
          target(conclusion),
        fillerSteps(stepIndex - 3) :+
          target(mainPremise) :+
          target(φ(a)) :+
          target(ψ(a)) :+
          existingStatementExtraction(
            Seq(known(Seq(assertion(combineConjunction, Seq(φ(a), ψ(a)), Nil)))),
            Seq(
              assertion(specification, Seq(Implication(Conjunction(φ($), ψ($)), χ($))), Seq(a)),
              assertion(modusPonens, Seq(Conjunction(φ(a), ψ(a)), χ(a)), Nil))))
    }
  }
}
