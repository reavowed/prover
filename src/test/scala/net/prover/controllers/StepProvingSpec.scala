package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model.TestDefinitions._
import net.prover.model.entries.Axiom
import net.prover.model.expressions.DefinedStatement
import net.prover.model.proof.Step

class StepProvingSpec extends ControllerSpec {
  def getBoundVariable(step: Step, path: Seq[Int]): String = {
    path.foldLeft(step.asInstanceOf[Step.Assertion].statement.asInstanceOf[DefinedStatement]) { case (s, i) =>
      s.components(i).asInstanceOf[DefinedStatement]
    }.scopedBoundVariableNames.head
  }

  "proving a step" should {
    "remove unnecessary structural simplifications" in {
      val service = createService
      val controller = new StepProvingController(service)

      val premise = Conjunction(φ, Implication(ψ, χ))

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definition(premise, Nil, Seq(extractRightConjunct, modusPonens)))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(ψ) :+ target(χ),
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(ψ) :+ assertion(modusPonens, Seq(ψ, χ), Nil))
    }
    "not remove necessary structural simplifications" in {
      val service = createService
      val controller = new StepProvingController(service)

      val premise = Implication(φ, Conjunction(ψ, χ))

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definition(premise, Nil, Seq(modusPonens, extractRightConjunct)))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(φ) :+ target(χ),
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(φ) :+ elided("Extracted", Seq(
          assertion(modusPonens, Seq(φ, Conjunction(ψ, χ)), Nil),
          assertion(extractRightConjunct, Seq(ψ, χ), Nil))))
    }

    "retain conclusion bound variable names" in {
      val service = createService
      val controller = new StepProvingController(service)

      val premise = ForAll("x")(Exists("y")(Equals($, $.^)))
      val statementToProve = Exists("z")(Equals($, a))

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definition(specification, Seq(Exists("y")(Equals($, $.^))), Seq(a), Nil))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 1) :+ target(premise) :+ target(statementToProve),
        beEqualTo(fillerSteps(stepIndex - 1) :+ target(premise) :+ assertion(specification, Seq(Exists("z")(Equals($, $.^))), Seq(a))) and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last, Nil)})
    }

    "retain conclusion bound variable names inside extraction" in {
      val service = createService
      val controller = new StepProvingController(service)

      val axiom = Axiom("Test Axiom", Nil, ForAll("x")(Equivalence(φ($), Exists("y")(ψ($.^, $)))))
      val entryContext = defaultEntryContext.copy(availableEntries = defaultEntryContext.availableEntries :+ axiom)

      val premise = φ(a)
      val statementToProve = Exists("z")(ψ(a, $))

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definition(axiom, Seq(φ($), ψ($(0), $(1))), Seq(a), Seq(specification, forwardImplicationFromEquivalence, modusPonens)))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 1) :+ target(premise) :+ target(statementToProve),
        beEqualTo(fillerSteps(stepIndex - 1) :+ target(premise) :+ elided(axiom, Seq(
          assertion(axiom, Seq(φ($), ψ($(0), $(1))), Nil),
          assertion(specification, Seq(Equivalence(φ($), Exists("z")(ψ($.^, $)))), Seq(a)),
          assertion(forwardImplicationFromEquivalence, Seq(φ(a), Exists("z")(ψ(a, $))), Nil),
          assertion(modusPonens, Seq(φ(a), Exists("z")(ψ(a, $))), Nil)))
        ) and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last.asInstanceOf[Step.Elided].substeps(0), Seq(0, 1))} and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last.asInstanceOf[Step.Elided].substeps(1), Seq(1))} and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last.asInstanceOf[Step.Elided].substeps(2), Seq(1))} and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last.asInstanceOf[Step.Elided].substeps(3), Nil)})(
        entryContext)
    }

    "retain premise bound variable names inside extraction" in {
      val service = createService
      val controller = new StepProvingController(service)

      val premise = ForAll("x")(ForAll("y")(φ($.^, $)))
      val statementToProve = φ(a, b)

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definition(premise, Seq(a, b), Seq(specification, specification)))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 1) :+ target(premise) :+ target(statementToProve),
        beEqualTo(fillerSteps(stepIndex - 1) :+ target(premise) :+ elided("Extracted", Seq(
          assertion(specification, Seq(ForAll("y")(φ($.^, $))), Seq(a)),
          assertion(specification, Seq(φ(a, $)), Seq(b))))
        ) and
          beEqualTo("y") ^^ {steps: Seq[Step] => getBoundVariable(steps.last.asInstanceOf[Step.Elided].substeps(0), Nil)})
    }
  }
}
