package net.prover.refactoring

import net.prover.StepBuilderHelper
import net.prover.model.TermVariablePlaceholder
import net.prover.model.TestDefinitions.{defaultAvailableEntries, getVariableDefinitions}
import org.specs2.mutable.Specification
import net.prover.model.TestDefinitions._

class ReproveSpec extends Specification with StepBuilderHelper {
  implicit val availableEntries = defaultAvailableEntries

  "reproving" should {
    "replace an existing statement extraction that starts with a deconstruction with an extraction from that deconstruction" in {
      val f = TermVariablePlaceholder("f", 0)
      val A = TermVariablePlaceholder("A", 1)
      val B = TermVariablePlaceholder("B", 2)
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(f -> 0, A -> 0, B -> 0))
      implicit val outerStepContext = createOuterStepContext(Nil)

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

      Reprove(createStepsWithContext(initialSteps)) mustEqual expectedSteps
    }
  }

}
