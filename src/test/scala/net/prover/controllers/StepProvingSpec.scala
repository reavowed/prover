package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model.TestDefinitions._
import net.prover.model.entries.Axiom
import net.prover.model.expressions.{DefinedStatement, TermVariable}
import net.prover.model.proof.{Step, StepProvingContext, SubstitutionContext}
import org.specs2.matcher.Matcher
import org.springframework.http.ResponseEntity

import scala.util.Success

class StepProvingSpec extends ControllerSpec {
  def getBoundVariable(step: Step, path: Seq[Int]): String = {
    path.foldLeft(step.provenStatement.get.asInstanceOf[DefinedStatement]) { case (s, i) =>
      s.components(i).asInstanceOf[DefinedStatement]
    }.boundVariableNames.head
  }

  def beResponseEntity[T](matcher: Matcher[T]): Matcher[ResponseEntity[_]] = matcher ^^ { (responseEntity: ResponseEntity[_]) => responseEntity.getBody.asInstanceOf[T] }

  "proving a step" should {
    "suggest an extraction using modus tollens" in {
      val service = mock[BookService]
      val controller = new StepProvingController(service)

      service.findStep[Step.Target](bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath)) returns Success((
        Step.Target(Negation(Equals(a, b))),
        entryContextAndStepContextToStepProvingContext(defaultEntryContext, createOuterStepContext(Nil, Nil))))

      controller.getPossibleInferencesForCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        "Singleton"
      ) should beResponseEntity(not(empty))
    }

    "remove unnecessary structural simplifications" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertionAndReplacement(service)
      val controller = new StepProvingController(service)

      val premise = Conjunction(φ, Implication(ψ, χ))

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithPremise(premise, Nil, Seq(extractRightConjunct, modusPonens), None))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(ψ) :+ target(χ),
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(ψ) :+ assertion(modusPonens, Seq(ψ, χ), Nil))
    }

    "not remove necessary structural simplifications" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertionAndReplacement(service)
      val controller = new StepProvingController(service)

      val premise = Implication(φ, Conjunction(ψ, χ))

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithPremise(premise, Nil, Seq(modusPonens, extractRightConjunct), None))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(φ) :+ target(χ),
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(φ) :+ elided("Extracted", Seq(
          assertion(modusPonens, Seq(φ, Conjunction(ψ, χ)), Nil),
          assertion(extractRightConjunct, Seq(ψ, χ), Nil))))
    }

    "retain conclusion bound variable names when proving target by inference" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertionAndReplacement(service)
      val controller = new StepProvingController(service)

      val premise = ForAll("x")(Exists("y")(Equals($, $.^)))
      val statementToProve = Exists("z")(Equals($, a))

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithInference(specification, Seq(Exists("y")(Equals($, $.^))), Seq(a), Nil))

      checkModifyStepsWithMatcher(
        service,
        fillerSteps(stepIndex - 1) :+ target(premise) :+ target(statementToProve),
        matchSteps(fillerSteps(stepIndex - 1) :+ target(premise) :+ assertion(specification, Seq(Exists("z")(Equals($, $.^))), Seq(a))) and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last, Nil)})
    }

    "retain conclusion bound variable names when proving target by inference inside extraction" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertionAndReplacement(service)
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
        definitionWithInference(axiom, Seq(φ($), ψ($(0), $(1))), Seq(a), Seq(specification, forwardImplicationFromEquivalence, modusPonens)))

      checkModifyStepsWithMatcher(
        service,
        fillerSteps(stepIndex - 1) :+ target(premise) :+ target(statementToProve),
        matchSteps(fillerSteps(stepIndex - 1) :+ target(premise) :+ elided(axiom, Seq(
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

    "retain conclusion bound variable names when adding target by inference" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertion(service)
      val controller = new StepProvingController(service)

      val premise = φ(b)

      controller.addNewTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithInference(existence, Seq(φ($)), Seq(b), Nil, conclusionOption = Some(Exists("z")(φ($)))))

      checkModifyStepsWithMatcher(
        service,
        fillerSteps(stepIndex - 1) :+ target(premise) :+ target(φ),
        matchSteps(fillerSteps(stepIndex - 1) :+ target(premise) :+ assertion(existence, Seq(φ($)), Seq(b)) :+ target(φ)) and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps(stepIndex), Nil)})
    }

    "retain conclusion bound variable names when adding target by premise" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertion(service)
      val controller = new StepProvingController(service)

      val premise = ForAll("x")(Implication(φ($), Exists("y")(ψ($.^, $))))

      controller.addNewTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithPremise(premise, Seq(a), Seq(specification, modusPonens), Some(Exists("z")(ψ(x, $)))))

      checkModifyStepsWithMatcher(
        service,
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(φ(a)) :+ target(χ(a)),
        matchSteps(fillerSteps(stepIndex - 2) :+ target(premise) :+ target(φ(a)) :+
          elided("Extracted", Seq(
            assertion(specification, Seq(Implication(φ($), Exists("z")(ψ($.^, $)))), Seq(a)),
            assertion(modusPonens, Seq(φ(a), Exists("z")(ψ(a, $))), Nil))) :+
          target(χ(a))
        ) and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps(stepIndex).asInstanceOf[Step.Elided].substeps(0), Seq(1))} and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps(stepIndex).asInstanceOf[Step.Elided].substeps(1), Nil)})
    }

    "retain premise bound variable names when proving target by inference" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertionAndReplacement(service)
      val controller = new StepProvingController(service)

      val statementToProve = Exists("y")(Equals($, a))

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithInference(specification, Seq(Exists("y")(Equals($, $.^))), Seq(a), Nil, premisesOption = Some(Seq(ForAll("z")(φ($))))))

      checkModifyStepsWithMatcher(
        service,
        fillerSteps(stepIndex) :+ target(statementToProve),
        matchSteps(fillerSteps(stepIndex) :+ target(ForAll("z")(Exists("y")(Equals($, $.^)))) :+ assertion(specification, Seq(Exists("z")(Equals($, $.^))), Seq(a))) and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps(stepIndex), Nil)})
    }

    "retain bound variable names in extraction premise when proving target by inference" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertionAndReplacement(service)
      val controller = new StepProvingController(service)

      val axiom = Axiom("Test Axiom", Nil, ForAll("x")(Equivalence(φ($), Exists("y")(ψ($.^, $)))))
      val entryContext = defaultEntryContext.copy(availableEntries = defaultEntryContext.availableEntries :+ axiom)

      val premise = Exists("z")(ψ(a, $))
      val statementToProve = φ(a)

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithInference(axiom, Seq(ψ($(0), $(1)), φ($)), Seq(a), Seq(specification, reverseImplicationFromEquivalence, modusPonens), premisesOption = Some(Seq(Exists("z")(ψ(x, $))))))

      checkModifyStepsWithMatcher(
        service,
        fillerSteps(stepIndex) :+ target(statementToProve),
        matchSteps(fillerSteps(stepIndex) :+ target(premise) :+ elided(axiom, Seq(
          assertion(axiom, Seq(φ($), ψ($(0), $(1))), Nil),
          assertion(specification, Seq(Equivalence(φ($), Exists("z")(ψ($.^, $)))), Seq(a)),
          assertion(reverseImplicationFromEquivalence, Seq(φ(a), Exists("z")(ψ(a, $))), Nil),
          assertion(modusPonens, Seq(Exists("z")(ψ(a, $)), φ(a)), Nil)))
        ) and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps(stepIndex), Nil)} and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last.asInstanceOf[Step.Elided].substeps(0), Seq(0, 1))} and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last.asInstanceOf[Step.Elided].substeps(1), Seq(1))} and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last.asInstanceOf[Step.Elided].substeps(2), Seq(0))})(
        entryContext)
    }

    "prove a target inside a scoped deduction" in {
      val service = mock[BookService]
      mockReplaceStepsForInsertionAndReplacement(service)
      val controller = new StepProvingController(service)

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithInference(successorOfNaturalIsNatural, Nil, Seq(add($.^, $)), Nil, unwrappers = Seq(ForAllDefinition, Implication, ForAllDefinition, Implication)))

      checkModifySteps(
        service,
        fillerSteps(stepIndex) :+ target(ForAll("x")(Implication(ElementOf($, A), ForAll("y")(Implication(ElementOf($, B), ElementOf(Successor(add($.^, $)), Naturals)))))),
        fillerSteps(stepIndex) :+
          target(ForAll("x")(Implication(ElementOf($, A), ForAll("y")(Implication(ElementOf($, B), ElementOf(add($.^, $), Naturals)))))) :+
          elided(successorOfNaturalIsNatural, Seq(
            generalization("x", Seq(
              deduction(ElementOf($, A), Seq(
                generalization("y", Seq(
                  deduction(ElementOf($, B), Seq(
                    elided("Extracted", Seq(
                      assertion(specification, Seq(Implication(ElementOf($.^^, A), ForAll("y")(Implication(ElementOf($, B), ElementOf(add($.^^^, $), Naturals))))), Seq($.^)),
                      assertion(modusPonens, Seq(ElementOf($.^, A), ForAll("y")(Implication(ElementOf($, B), ElementOf(add($.^^, $), Naturals)))), Nil),
                      assertion(specification, Seq(Implication(ElementOf($.^^, B), ElementOf(add($.^, $.^^), Naturals))), Seq($)),
                      assertion(modusPonens, Seq(ElementOf($, B), ElementOf(add($.^, $), Naturals)), Nil))),
                    assertion(successorOfNaturalIsNatural, Nil, Seq(add($.^, $))))))))))))))
    }
  }
}
