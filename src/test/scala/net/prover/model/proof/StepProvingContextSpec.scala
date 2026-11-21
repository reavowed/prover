package net.prover.model.proof

import net.prover.ContextHelper
import net.prover.model.*
import net.prover.model.TestDefinitions.{a, b, *}
import net.prover.model.expressions.Statement
import net.prover.model.proof.StepProvingContext
import net.prover.proving.derivation.SimpleDerivationStep.inference
import org.specs2.mutable.Specification

class StepProvingContextSpec extends Specification with ContextHelper {
  val OrderedPair = simpleTermDefinition("orderedPair", Seq(a, b), Format.Explicit("(%1, %2)", "(a, b)", 3, false, false))
  val firstCoordinatesOfEqualOrderedPairsAreEqual = createInference("First Coordinates of Equal Ordered Pairs Are Equal", Seq(Equals(OrderedPair(a, b), OrderedPair(c, d))), Equals(a, c))
  val secondCoordinatesOfEqualOrderedPairsAreEqual = createInference("Second Coordinates of Equal Ordered Pairs Are Equal", Seq(Equals(OrderedPair(a, b), OrderedPair(c, d))), Equals(b, d))

  given availableEntries: AvailableEntries = defaultAvailableEntriesPlus(OrderedPair, firstCoordinatesOfEqualOrderedPairsAreEqual, secondCoordinatesOfEqualOrderedPairsAreEqual)
  given variableDefinitions: VariableDefinitions = getVariableDefinitions(Nil, Seq(a -> 0, b -> 0, c -> 0, d -> 0))

  "known statements from premises" should {
    def getDerivationInferenceNames(premises: Seq[Statement], knownStatement: Statement): Option[Seq[String]] = {
      given stepContext: StepContext = createBaseStepContext(premises)
      implicitly[StepProvingContext].knownStatementsFromPremisesBySerializedStatement
        .get(knownStatement.serializedForHash)
        .map(_.derivation.steps.map(_.inference.name))
    }

    val pairEquality = Equals(OrderedPair(a, b), OrderedPair(c, d))

    "simplify an equality premise without reversing it" in {
      getDerivationInferenceNames(Seq(pairEquality), Equals(a, c)) must beSome(Seq(firstCoordinatesOfEqualOrderedPairsAreEqual.name))
      getDerivationInferenceNames(Seq(pairEquality), Equals(b, d)) must beSome(Seq(secondCoordinatesOfEqualOrderedPairsAreEqual.name))
    }

    "apply a reversal after simplifying an equality premise, rather than before" in {
      getDerivationInferenceNames(Seq(pairEquality), Equals(c, a)) must beSome(Seq(firstCoordinatesOfEqualOrderedPairsAreEqual.name, reverseEquality.name))
      getDerivationInferenceNames(Seq(pairEquality), Equals(d, b)) must beSome(Seq(secondCoordinatesOfEqualOrderedPairsAreEqual.name, reverseEquality.name))
    }
  }
}
