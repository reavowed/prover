package net.prover.proving

import net.prover.{BookServiceHelper, StepHelpers}
import net.prover.controllers.BookService
import net.prover.controllers.models.{PathData, SerializedSubstitutions, StepDefinition}
import net.prover.model.TestDefinitions._
import net.prover.proving.extraction.ExtractionDefinition
import org.specs2.mutable.Specification

class ProveCurrentTargetSpec extends Specification with BookServiceHelper with StepHelpers {
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
          ExtractionDefinition.Empty.serialized,
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

    "replace target with an inference extraction containing a deconstruction" in {
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(a -> 0, b -> 0))

      implicit val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)

      ProveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        StepDefinition(
          Some(integerAdditionIsCommutative.id),
          None,
          SerializedSubstitutions(Nil, Seq(a.serialized, b.serialized)),
          ExtractionDefinition(
            Seq(
              Commutative.deconstructionInference.summary,
              extractRightConjunct.summary,
              specification.summary,
              modusPonens.summary,
              specification.summary,
              modusPonens.summary),
            None
          ).serialized,
          Nil,
          None,
          None,
          None))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 2) :+
          target(ElementOf(a, BaseSet(IntegerAddition))) :+
          target(ElementOf(b, BaseSet(IntegerAddition))) :+
          target(Equals(addZ(a, b), addZ(b, a))),
        fillerSteps(stepIndex - 2) :+
          target(ElementOf(a, BaseSet(IntegerAddition))) :+
          target(ElementOf(b, BaseSet(IntegerAddition))) :+
          inferenceExtraction(Seq(
            assertion(integerAdditionIsCommutative, Nil, Nil),
            inferenceExtraction(Seq(
              assertion(Commutative.deconstructionInference, Nil, Seq(IntegerAddition)),
              assertion(
                specification,
                Seq(Implication(ElementOf($, BaseSet(IntegerAddition)), ForAllIn("b", BaseSet(IntegerAddition))(Equals(Apply(IntegerAddition, Pair($.^, $)), Apply(IntegerAddition, Pair($, $.^)))))),
                Seq(a)),
              assertion(
                modusPonens,
                Seq(ElementOf(a, BaseSet(IntegerAddition)), ForAllIn("b", BaseSet(IntegerAddition))(Equals(Apply(IntegerAddition, Pair(a, $)), Apply(IntegerAddition, Pair($, a))))),
                Nil),
              assertion(
                specification,
                Seq(Implication(ElementOf($, BaseSet(IntegerAddition)), Equals(Apply(IntegerAddition, Pair(a, $)), Apply(IntegerAddition, Pair($, a))))),
                Seq(b)),
              assertion(
                modusPonens,
                Seq(ElementOf(b, BaseSet(IntegerAddition)), Equals(Apply(IntegerAddition, Pair(a, b)), Apply(IntegerAddition, Pair(b, a)))),
                Nil))))))
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
          ExtractionDefinition.Empty.serialized,
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
          premiseDerivation(Seq(
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
          ExtractionDefinition.Empty.serialized,
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
          ExtractionDefinition.Empty.serialized,
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

    "find a wrapped premise for a wrapped inference" in {
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 1), Seq(a -> 0))

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
          SerializedSubstitutions(Seq(φ($).serialized, ψ($).serialized), Nil),
          ExtractionDefinition.Empty.serialized,
          Seq(ForAllDefinition.symbol),
          None,
          None,
          None))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 2) :+
          target(ForAll("x")(Implication(φ($), ψ($)))) :+
          target(ForAll("x")(φ($))) :+
          target(ForAll("x")(ψ($))),
        fillerSteps(stepIndex - 2) :+
          target(ForAll("x")(Implication(φ($), ψ($)))) :+
          target(ForAll("x")(φ($))) :+
          wrappedInferenceApplication(Seq(
            generalization("x", Seq(
              assertion(
                specification,
                Seq(Implication(φ($.^), ψ($.^))),
                Seq($)),
              assertion(
                specification,
                Seq(φ($.^)),
                Seq($)),
              assertion(
                modusPonens,
                Seq(φ($), ψ($)),
                Nil))))))
    }

    "find a wrapped premise for a wrapped inference that references a bound variable" in {
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 1), Seq(a -> 0))

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
          SerializedSubstitutions(Seq(φ($).serialized, ψ($.^).serialized), Nil),
          ExtractionDefinition.Empty.serialized,
          Seq(ForAllDefinition.symbol),
          None,
          None,
          None))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 2) :+
          target(ForAll("x")(Implication(φ($), ψ($.^)))) :+
          target(ForAll("x")(φ($))) :+
          target(ForAll("x")(ψ($.^))),
        fillerSteps(stepIndex - 2) :+
          target(ForAll("x")(Implication(φ($), ψ($.^)))) :+
          target(ForAll("x")(φ($))) :+
          wrappedInferenceApplication(Seq(
            generalization("x", Seq(
              assertion(
                specification,
                Seq(Implication(φ($.^^), ψ($.^))),
                Seq($)),
              assertion(
                specification,
                Seq(φ($.^^)),
                Seq($)),
              assertion(
                modusPonens,
                Seq(φ($), ψ($.^)),
                Nil))))),
        Seq("x"))
    }

    "prove using an inference whose premises have been added as generalized implications" in {
      val axiom = createInference(
        "Two Empty Sets Are Equal",
        Seq(ForAll("x")(Negation(ElementOf($, a))), ForAll("x")(Negation(ElementOf($, b)))),
        Equals(a, b))
      implicit val availableEntries = defaultAvailableEntries.addEntry(axiom)
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 1), Seq(a -> 0))
      implicit val service = mock[BookService]
      mockReplaceStepsForSimpleReplacement(service)

      ProveCurrentTarget(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        StepDefinition(
          Some(axiom.id),
          None,
          SerializedSubstitutions(Nil, Seq($.^.serialized, $.serialized)),
          ExtractionDefinition.Empty.serialized,
          Seq(ForAllDefinition.symbol, ForAllDefinition.symbol, Implication.symbol),
          None,
          None,
          None))

      checkModifySteps(
        service,
        fillerSteps(stepIndex) :+
          target(ForAll("x")(ForAll("y")(Implication(Conjunction(ForAll("z")(Negation(ElementOf($, $.^^))), ForAll("x")(Negation(ElementOf($, $.^)))), Equals($.^, $))))),
        fillerSteps(stepIndex) :+
          wrappedInferenceApplication(Seq(
            generalization("x", Seq(
              generalization("y", Seq(
                deduction(Conjunction(ForAll("z")(Negation(ElementOf($, $.^^))), ForAll("x")(Negation(ElementOf($, $.^)))), Seq(
                  assertion(
                    axiom,
                    Nil,
                    Seq($.^, $)))))))))))
    }
  }
}
