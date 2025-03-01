package net.prover.controllers

import net.prover.controllers.models.PathData
import net.prover.model.Substitutions
import net.prover.model.TestDefinitions._
import net.prover.model.expressions.{DefinedStatement, TermVariable}
import net.prover.model.proof.{Step, StepLike, SubstitutionContext}
import net.prover.proving.extraction.ExtractionDefinition
import org.specs2.matcher.Matcher
import org.springframework.http.ResponseEntity

import scala.util.Success

class StepProvingSpec extends ControllerSpec {
  def getBoundVariable(step: StepLike, path: Seq[Int]): String = {
    path.foldLeft(step.statement.asInstanceOf[DefinedStatement]) { case (s, i) =>
      s.components(i).asInstanceOf[DefinedStatement]
    }.boundVariableNames.head
  }

  def beResponseEntity[T](matcher: Matcher[T]): Matcher[ResponseEntity[_]] = matcher ^^ { (responseEntity: ResponseEntity[_]) => responseEntity.getBody.asInstanceOf[T] }

  implicit val availableEntries = defaultAvailableEntries
  implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 0, χ -> 0, ω -> 0), Seq(a -> 0, b -> 0))

  "proving a step" should {

    "suggest an extraction using modus tollens" in {
      implicit val service = mock[BookService]
      val controller = new StepProvingController

      service.findStep[Step.TargetStep](bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath)) returns Success(
        createTargetStepWithContext(
          Negation(Equals(a, b)))(
          createOuterStepContext(Nil),
          implicitly))

      controller.suggestInferencesForExistingTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        "Singleton"
      ) should beResponseEntity(not(empty))
    }

    "remove unnecessary structural simplifications" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndReplacement(service)
      val controller = new StepProvingController

      val premise = Conjunction(φ, Implication(ψ, χ))

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithPremise(premise, Nil, simpleExtraction(extractRightConjunct, modusPonens), None))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(ψ) :+ target(χ),
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(ψ) :+ assertion(modusPonens, Seq(ψ, χ), Nil))
    }

    "not remove necessary structural simplifications" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndReplacement(service)
      val controller = new StepProvingController

      val premise = Implication(φ, Conjunction(ψ, χ))

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithPremise(premise, Nil, simpleExtraction(modusPonens, extractRightConjunct), None))

      val x: SubstitutionContext => Seq[Step.AssertionStep] = Seq(
        assertion(modusPonens, Seq(φ, Conjunction(ψ, χ)), Nil),
        assertion(extractRightConjunct, Seq(ψ, χ), Nil)
      )

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(φ) :+ target(χ),
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(φ) :+ existingStatementExtraction(Seq(
          assertion(modusPonens, Seq(φ, Conjunction(ψ, χ)), Nil),
          assertion(extractRightConjunct, Seq(ψ, χ), Nil))))
    }

    "retain conclusion bound variable names when proving target by inference" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndReplacement(service)
      val controller = new StepProvingController

      val premise = ForAll("x")(Exists("y")(Equals($, $.^)))
      val statementToProve = Exists("z")(Equals($, a))

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithInference(specification, Seq(Exists("y")(Equals($, $.^))), Seq(a), ExtractionDefinition.Empty))

      checkModifyStepsWithMatcher(
        service,
        fillerSteps(stepIndex - 1) :+ target(premise) :+ target(statementToProve),
        matchSteps(fillerSteps(stepIndex - 1) :+ target(premise) :+ assertion(specification, Seq(Exists("z")(Equals($, $.^))), Seq(a))) and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last, Nil)})
    }

    "retain conclusion bound variable names when proving target by inference inside extraction" in {
      val axiom = createInference("Test Axiom", Nil, ForAll("x")(Equivalence(φ($), Exists("y")(ψ($.^, $)))))
      implicit val availableEntries = defaultAvailableEntriesPlus(axiom)
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 2), Seq(a -> 0))

      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndReplacement(service)
      val controller = new StepProvingController

      val premise = φ(a)
      val statementToProve = Exists("z")(ψ(a, $))

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithInference(axiom, Seq(φ($), ψ($(0), $(1))), Seq(a), simpleExtraction(specification, forwardImplicationFromEquivalence, modusPonens)))

      checkModifyStepsWithMatcher(
        service,
        fillerSteps(stepIndex - 1) :+ target(premise) :+ target(statementToProve),
        matchSteps(fillerSteps(stepIndex - 1) :+ target(premise) :+ inferenceExtraction(
          assertion(axiom, Seq(φ($), ψ($(0), $(1))), Nil),
          Seq(
            assertion(specification, Seq(Equivalence(φ($), Exists("z")(ψ($.^, $)))), Seq(a)),
            assertion(forwardImplicationFromEquivalence, Seq(φ(a), Exists("z")(ψ(a, $))), Nil),
            assertion(modusPonens, Seq(φ(a), Exists("z")(ψ(a, $))), Nil)))
        ) and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last.asInstanceOf[Step.InferenceExtractionStep].inferenceExtraction.assertionStep, Seq(0, 1))} and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last.asInstanceOf[Step.InferenceExtractionStep].inferenceExtraction.extraction.extractionSteps(0), Seq(1))} and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last.asInstanceOf[Step.InferenceExtractionStep].inferenceExtraction.extraction.extractionSteps(1), Seq(1))} and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last.asInstanceOf[Step.InferenceExtractionStep].inferenceExtraction.extraction.extractionSteps(2), Nil)})
    }

    "retain conclusion bound variable names when adding target by inference" in {
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 0), Seq(a -> 0, b -> 0))

      implicit val service = mock[BookService]
      mockReplaceStepsForInsertion(service)
      val controller = new StepProvingController

      val premise = φ(b)

      controller.addNewTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithInference(valueForExistence, Seq(φ($)), Seq(b), ExtractionDefinition.Empty, conclusionOption = Some(Exists("z")(φ($)))))

      checkModifyStepsWithMatcher(
        service,
        fillerSteps(stepIndex - 1) :+ target(premise) :+ target(ψ),
        matchSteps(fillerSteps(stepIndex - 1) :+ target(premise) :+ assertion(valueForExistence, Seq(φ($)), Seq(b)) :+ target(ψ)) and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps(stepIndex), Nil)})
    }

    "retain conclusion bound variable names when adding target by premise" in {
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 2, χ -> 1), Seq(a -> 0))
      val x = TermVariable(1, Nil)

      implicit val service = mock[BookService]
      mockReplaceStepsForInsertion(service)
      val controller = new StepProvingController

      val premise = ForAll("x")(Implication(φ($), Exists("y")(ψ($.^, $))))

      controller.addNewTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithPremise(premise, Seq(a()), simpleExtraction(specification, modusPonens), Some(Exists("z")(ψ(x, $)))))

      checkModifyStepsWithMatcher(
        service,
        fillerSteps(stepIndex - 2) :+ target(premise) :+ target(φ(a)) :+ target(χ(a)),
        matchSteps(fillerSteps(stepIndex - 2) :+ target(premise) :+ target(φ(a)) :+
          existingStatementExtraction(Seq(
            assertion(specification, Seq(Implication(φ($), Exists("z")(ψ($.^, $)))), Seq(a)),
            assertion(modusPonens, Seq(φ(a), Exists("z")(ψ(a, $))), Nil))) :+
          target(χ(a))
        ) and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps(stepIndex).asInstanceOf[Step.ExistingStatementExtractionStep].extraction.toProofSteps(0), Seq(1))} and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps(stepIndex).asInstanceOf[Step.ExistingStatementExtractionStep].extraction.toProofSteps(1), Nil)})
    }

    "retain premise bound variable names when proving target by inference" in {
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0))
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndReplacement(service)
      val controller = new StepProvingController

      val statementToProve = Exists("y")(Equals($, a))

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithInference(specification, Seq(Exists("y")(Equals($, $.^))), Seq(a), ExtractionDefinition.Empty, premisesOption = Some(Seq(ForAll("z")(φ($))))))

      checkModifyStepsWithMatcher(
        service,
        fillerSteps(stepIndex) :+ target(statementToProve),
        matchSteps(fillerSteps(stepIndex) :+ target(ForAll("z")(Exists("y")(Equals($, $.^)))) :+ assertion(specification, Seq(Exists("z")(Equals($, $.^))), Seq(a))) and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps(stepIndex), Nil)})
    }

    "retain bound variable names in extraction premise when proving target by inference" in {
      val axiom = createInference("Test Axiom", Nil, ForAll("x")(Equivalence(φ($), Exists("y")(ψ($.^, $)))))
      implicit val availableEntries = defaultAvailableEntriesPlus(axiom)
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 2), Seq(a -> 0, b -> 0, c -> 0))
      val x = TermVariable(0, Nil) // variable that will be generated when specifying the axiom

      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndReplacement(service)
      val controller = new StepProvingController

      val premise = Exists("z")(ψ(a, $))
      val statementToProve = φ(a)

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithInference(axiom, Seq(φ($), ψ($(0), $(1))), Seq(a), simpleExtraction(specification, reverseImplicationFromEquivalence, modusPonens), premisesOption = Some(Seq(Exists("z")(ψ(x, $))))))

      checkModifyStepsWithMatcher(
        service,
        fillerSteps(stepIndex - 1) :+ target(ψ(b, c)) :+ target(statementToProve),
        matchSteps(fillerSteps(stepIndex - 1) :+ target(ψ(b, c)) :+ target(premise) :+ inferenceExtraction(
          assertion(axiom, Seq(φ($), ψ($(0), $(1))), Nil),
          Seq(
            assertion(specification, Seq(Equivalence(φ($), Exists("z")(ψ($.^, $)))), Seq(a)),
            assertion(reverseImplicationFromEquivalence, Seq(φ(a), Exists("z")(ψ(a, $))), Nil),
            assertion(modusPonens, Seq(Exists("z")(ψ(a, $)), φ(a)), Nil)))
        ) and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps(stepIndex), Nil)} and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last.asInstanceOf[Step.InferenceExtractionStep].inferenceExtraction.assertionStep, Seq(0, 1))} and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last.asInstanceOf[Step.InferenceExtractionStep].inferenceExtraction.extraction.extractionSteps(0), Seq(1))} and
          beEqualTo("z") ^^ {steps: Seq[Step] => getBoundVariable(steps.last.asInstanceOf[Step.InferenceExtractionStep].inferenceExtraction.extraction.extractionSteps(1), Seq(0))})
    }

    "prove a target inside a scoped deduction" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndReplacement(service)
      val controller = new StepProvingController

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithInference(successorOfNaturalIsNatural, Nil, Seq(add($.^, $)), ExtractionDefinition.Empty, unwrappers = Seq(ForAllDefinition, Implication, ForAllDefinition, Implication)))

      checkModifySteps(
        service,
        fillerSteps(stepIndex) :+
          target(ForAll("x")(Implication(ElementOf($, a), ForAll("y")(Implication(ElementOf($, b), ElementOf(Successor(add($.^, $)), Naturals)))))),
        fillerSteps(stepIndex) :+
          target(ForAll("x")(Implication(ElementOf($, a), ForAll("y")(Implication(ElementOf($, b), ElementOf(add($.^, $), Naturals)))))) :+
          wrappedInferenceApplication(Seq(
            generalization("x", Seq(
              deduction(ElementOf($, a), Seq(
                generalization("y", Seq(
                  deduction(ElementOf($, b), Seq(
                    existingStatementExtraction(Seq(
                      assertion(specification, Seq(Implication(ElementOf($.^^, a), ForAll("y")(Implication(ElementOf($, b), ElementOf(add($.^^^, $), Naturals))))), Seq($.^)),
                      assertion(modusPonens, Seq(ElementOf($.^, a), ForAll("y")(Implication(ElementOf($, b), ElementOf(add($.^^, $), Naturals)))), Nil),
                      assertion(specification, Seq(Implication(ElementOf($.^^, b), ElementOf(add($.^, $.^^), Naturals))), Seq($)),
                      assertion(modusPonens, Seq(ElementOf($, b), ElementOf(add($.^, $), Naturals)), Nil))),
                    assertion(successorOfNaturalIsNatural, Nil, Seq(add($.^, $))))))))))))))
    }

    "group extractions by definition application in a nice way" in {
      val additionProperty = Conjunction(
        ForAllIn("a", Naturals)(Equals(add($, Zero), $)),
        ForAllIn("a", Naturals)(ForAllIn("b", Naturals)(Equals(add($.^, Successor($)), Successor(add($.^, $))))))
      val axiom = createInference(
        "Function Properties of Natural Addition",
        Nil,
        Conjunction(
          Conjunction(
            Function(Addition),
            FunctionFrom(Addition, Product(Naturals, Naturals), Naturals)),
          additionProperty))
      implicit val availableEntries = defaultAvailableEntriesPlus(axiom)

      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndReplacement(service)
      val controller = new StepProvingController

      controller.proveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithInference(axiom, Nil, Nil, simpleExtraction(extractLeftConjunct, extractRightConjunct, FunctionFrom.statementDefinition.deconstructionInference.get, extractRightConjunct, extractLeftConjunct)))

      checkModifySteps(
        service,
        fillerSteps(stepIndex) :+ target(Equals(Domain(Addition), Product(Naturals, Naturals))),
        fillerSteps(stepIndex) :+
          inferenceExtraction(
            assertion(axiom, Nil, Nil),
            Seq(
              inferenceExtraction(
                assertion(FunctionFrom.statementDefinition.deconstructionInference.get, Nil, Seq(Addition, Product(Naturals, Naturals), Naturals)),
                Seq(
                  assertion(extractRightConjunct, Seq(Function(Addition), Conjunction(Equals(Domain(Addition), Product(Naturals, Naturals)), Subset(Range(Addition), Naturals))), Nil),
                  assertion(extractLeftConjunct, Seq(Equals(Domain(Addition), Product(Naturals, Naturals)), Subset(Range(Addition), Naturals)), Nil))))))
    }

    "prove a new target by extracting inside a bound variable" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertion(service)
      val controller = new StepProvingController

      val premise = ForAll("x")(φ($))
      val localVariableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 0), Nil)

      controller.addNewTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithPremise(premise, simpleExtraction(specification), Substitutions(Seq(φ($), ψ), Seq($)), Some(φ($)))(implicitly, localVariableDefinitions))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 1)
          :+ target(premise)
          :+ target(ψ),
        fillerSteps(stepIndex - 1)
          :+ target(premise)
          :+ assertion(specification, Seq(φ($^)), Seq($))
          :+ target(ψ),
        Seq("x"))(
        implicitly,
        localVariableDefinitions)
    }
  }
}
