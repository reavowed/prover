package net.prover.refactoring

import net.prover.StepBuilderHelper
import net.prover.model.TestDefinitions._
import net.prover.model.proof.SubstitutionContext
import org.specs2.mutable.Specification

class ReplaceInferenceSpec extends Specification with StepBuilderHelper {
  "replace inference" should {
    "replace a simple assertion in a rewrite with an extraction" in {
      implicit val variableDefinitions = getVariableDefinitions(Nil, Seq(a -> 0, b -> 0))

      val simpleIntegerAdditionIsCommutative = createInference(
        "Integer Addition is Commutative",
        Seq(ElementOf(a, Integers), ElementOf(b, Integers)),
        Equals(addZ(a, b), addZ(b, a)))
      implicit val availableEntries = defaultAvailableEntries.addEntry(simpleIntegerAdditionIsCommutative)
      implicit val stepContext = createBaseStepContext(Seq(
        ElementOf(a, Integers),
        ElementOf(b, Integers)))

      val initialStep = recalculateReferences(rewriteStep(
        known(Seq(assertion(simpleIntegerAdditionIsCommutative, Nil, Seq(a, b)))),
        assertion(equivalenceOfSubstitutedEquals, Seq(φ($)), Seq(addZ(a, b), addZ(b, a)))
      )(SubstitutionContext.outsideProof))

      val expectedStep = recalculateReferences(rewriteStep(
        Seq(
          known(Seq(
            inferenceExtraction(
              assertion(IntegerAdditionDefinition.definitionInference, Nil, Nil),
              Seq(inferenceExtraction(
                assertion(BinaryOperationOn.deconstructionInference, Nil, Seq(IntegerAddition, Integers)),
                Seq(assertion(reverseEquality, Nil, Seq(BaseSet(IntegerAddition), Integers)))))),
            assertion(substitutionOfEquals, Seq(ElementOf(a, $)), Seq(Integers, BaseSet(IntegerAddition))))),
          known(Seq(
            inferenceExtraction(
              assertion(IntegerAdditionDefinition.definitionInference, Nil, Nil),
              Seq(inferenceExtraction(
                assertion(BinaryOperationOn.deconstructionInference, Nil, Seq(IntegerAddition, Integers)),
                Seq(assertion(reverseEquality, Nil, Seq(BaseSet(IntegerAddition), Integers)))))),
            assertion(substitutionOfEquals, Seq(ElementOf(b, $)), Seq(Integers, BaseSet(IntegerAddition)))))),
        inferenceExtraction(
          assertion(integerAdditionIsCommutative, Nil, Nil),
          Seq(
            inferenceExtraction(
              assertion(Commutative.deconstructionInference, Nil, Seq(IntegerAddition)),
              Seq(
                assertion(specification, Seq(Implication(ElementOf($, BaseSet(IntegerAddition)), ForAllIn("b", BaseSet(IntegerAddition))(Equals(addZ($.^, $), addZ($, $.^))))), Seq(a)),
                assertion(modusPonens, Seq(ElementOf(a, BaseSet(IntegerAddition)), ForAllIn("b", BaseSet(IntegerAddition))(Equals(addZ(a, $), addZ($, a)))), Nil),
                assertion(specification, Seq(Implication(ElementOf($, BaseSet(IntegerAddition)), Equals(addZ(a, $), addZ($, a)))), Seq(b)),
                assertion(modusPonens, Seq(ElementOf(b, BaseSet(IntegerAddition)), Equals(addZ(a, b), addZ(b, a))), Nil))))),
        assertion(equivalenceOfSubstitutedEquals, Seq(φ($)), Seq(addZ(a, b), addZ(b, a)))
      )(SubstitutionContext.outsideProof))

      ReplaceInference(
        simpleIntegerAdditionIsCommutative,
        integerAdditionIsCommutative)(
        createStepWithContext(initialStep)
      ) must beSuccessfulTry(expectedStep)
    }
  }

}
