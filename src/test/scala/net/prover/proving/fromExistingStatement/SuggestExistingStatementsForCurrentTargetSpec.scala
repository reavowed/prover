package net.prover.proving.fromExistingStatement

import net.prover.ContextHelper
import net.prover.controllers.BookService
import net.prover.controllers.models.{PathData, PossibleConclusionWithPremises, SuggestedSubstitutions}
import net.prover.model.TestDefinitions.*
import net.prover.model.{AvailableEntries, VariableDefinitions}
import net.prover.model.proof.{Step, StepContext}
import org.specs2.mutable.Specification

import scala.util.Success

class SuggestExistingStatementsForCurrentTargetSpec extends Specification with ContextHelper with PossibleConclusionMatchers {
  given availableEntries: AvailableEntries = defaultAvailableEntries

  "SuggestExistingStatementsForCurrentTarget" should {
    "suggest all extractions from an existing statement" in {
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 1), Seq(a -> 0))
      val premise = ForAll("x")(Implication(φ($), ψ($)))
      val target = Negation(φ(a))
      given service: BookService = mock[BookService]
      given stepContext: StepContext = createBaseStepContext(Seq(premise))
      service.findStep[Step.TargetStep](bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath)) returns
        Success(createTargetStepWithContext(target))

      val result = SuggestExistingStatementsForCurrentTarget(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), premise.serialized)

      result must beSuccessfulTry(contain[PossibleConclusionWithPremises](exactly(
        bePossibleConclusionWithPremises(Negation(φ(b)), Seq(Negation(ψ(b))))
      )))
    }

    "suggest substitutions at correct depth" in {
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 1), Seq(a -> 0))
      val premise = ForAll("y")(Implication(φ($), ψ($)))
      val target = ψ(a)
      given service: BookService = mock[BookService]
      given stepContext: StepContext = createBaseStepContext(Seq(premise), Seq("x"))
      service.findStep[Step.TargetStep](bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath)) returns
        Success(createTargetStepWithContext(target))

      val result = SuggestExistingStatementsForCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        premise.insertExternalParameters(1).serialized)

      result must beSuccessfulTry(contain[PossibleConclusionWithPremises](exactly(
        bePossibleConclusionWithSubstitutions(SuggestedSubstitutions(
          Seq(Some(φ($.^)), Some(ψ($.^))),
          Seq(Some(a), Some(a)),
          Seq(Nil, Nil),
          Seq(Nil, Nil)))
      )))
    }
  }
}
