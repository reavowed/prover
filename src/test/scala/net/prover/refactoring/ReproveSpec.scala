package net.prover.refactoring

import net.prover.StepBuilderHelper
import net.prover.model.TestDefinitions.{defaultAvailableEntries, getVariableDefinitions, *}
import net.prover.model.proof.StepContext
import net.prover.model.{AvailableEntries, TermVariablePlaceholder, VariableDefinitions}
import org.specs2.mutable.Specification

class ReproveSpec extends Specification with StepBuilderHelper {
  given availableEntries: AvailableEntries = defaultAvailableEntries

  "reproving" should {
    "replace an existing statement extraction that starts with a deconstruction with an extraction from that deconstruction" in {
      val f = TermVariablePlaceholder("f", 0)
      val A = TermVariablePlaceholder("A", 1)
      val B = TermVariablePlaceholder("B", 2)
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Nil, Seq(f -> 0, A -> 0, B -> 0))
      given outerStepContext: StepContext = createOuterStepContext(Nil)

      val initialSteps = recalculateReferences(Seq(
        target(Conjunction(Function(f), FunctionFrom(f, A, B))),
        existingStatementExtraction(Seq(
          assertion(FunctionFrom.deconstructionInference, Nil, Seq(f, A, B)),
          assertion(extractRightConjunct, Seq(Function(f), Conjunction(Equals(Domain(f), A), Subset(Range(f), B))), Nil)))
      )(outerStepContext))
      val expectedSteps = recalculateReferences(Seq(
        target(Conjunction(Function(f), FunctionFrom(f, A, B))),
        inferenceExtraction(
          assertion(FunctionFrom.deconstructionInference, Nil, Seq(f, A, B)),
          Seq(
            assertion(extractRightConjunct, Seq(Function(f), Conjunction(Equals(Domain(f), A), Subset(Range(f), B))), Nil)))
      )(outerStepContext))

      Reprove(createStepsWithContext(initialSteps)) must beEqualTo(expectedSteps)
    }

    "replace an assertion for Substitution of Equals that has a single premise with a rewrite" in {
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0))
      given outerStepContext: StepContext = createOuterStepContext(Nil)

      val initialSteps = recalculateReferences(Seq(
        target(φ(a)),
        premiseDerivation(
          Seq(known(Seq(assertion(zeroIsLeftIdentityForAddition, Nil, Seq(a))))),
          assertion(substitutionOfEquals, Seq(φ($)), Seq(a, add(Zero, a))))
      )(outerStepContext))
      val expectedSteps = recalculateReferences(Seq(
        target(φ(a)),
        rewriteStep(
          known(Seq(assertion(zeroIsLeftIdentityForAddition, Nil, Seq(a)))),
          assertion(substitutionOfEquals, Seq(φ($)), Seq(a, add(Zero, a))))
      )(outerStepContext))

      Reprove(createStepsWithContext(initialSteps)) must beEqualTo(expectedSteps)
    }
  }

}
