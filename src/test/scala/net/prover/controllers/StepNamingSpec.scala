package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model.{AvailableEntries, VariableDefinitions}
import net.prover.model.TestDefinitions.*
import net.prover.model.proof.Step
import net.prover.proving.extraction.ExtractionDefinition

class StepNamingSpec extends ControllerSpec {

  val namingInference = createInference(
    "Value for Existence",
    Seq(Exists("x")(φ($)), ForAll("x")(Implication(φ($), ψ))),
    ψ)
  val decomposeIntoPair = createInference(
    "Decompose into Pair",
    Nil,
    ExistsIn("x", a)(ExistsIn("y", b)(Equals(c, Pair($.^, $)))))

  given availableEntries: AvailableEntries = defaultAvailableEntriesPlus(namingInference, decomposeIntoPair)
  given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0))

  "introducing naming by inference" should {
    "give each nested naming step a conclusion in its own context" in {
      given service: BookService = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)

      val controller = new StepNamingController(service)

      // The target refers to the outer bound variable "z", so its representation
      // must shift by one level for each naming step it is nested inside.
      controller.introduceNamingByInference(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithInference(decomposeIntoPair, Nil, Seq(a, b, $), ExtractionDefinition.Empty))

      checkModifyStepsWithoutPropsWithMatcher(
        service,
        fillerSteps(stepIndex) :+ target(φ($)),
        beLike[Seq[Step]] {
          case _ :+ (_: Step.AssertionStep) :+ (outer: Step.NamingStep) =>
            outer.statement must beEqualTo(φ($))
            outer.substeps must beLike {
              case Seq(inner: Step.NamingStep) =>
                inner.statement must beEqualTo(φ($.^))
                inner.substeps must beEqualTo(Seq(Step.TargetStep(φ($.^^))))
            }
        },
        Seq("z"))
    }
  }
}
