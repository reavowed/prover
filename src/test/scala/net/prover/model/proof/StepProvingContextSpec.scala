package net.prover.model.proof

import net.prover.{ContextHelper, StepHelpers}
import net.prover.model.*
import net.prover.model.TestDefinitions.{a, b, *}
import net.prover.model.expressions.Statement
import net.prover.model.proof.StepProvingContext
import net.prover.proving.derivation.DerivedStatement
import net.prover.proving.derivation.SimpleDerivationStep.inference
import net.prover.proving.rewrite.RewritePremise
import net.prover.theorems.{DisplayStep, GetDisplaySteps}
import org.specs2.mutable.Specification

class StepProvingContextSpec extends Specification with ContextHelper with StepHelpers {
  val OrderedPair = simpleTermDefinition("orderedPair", Seq(a, b), Format.Explicit("(%1, %2)", "(a, b)", 3, false, false))
  val firstCoordinatesOfEqualOrderedPairsAreEqual = createInference("First Coordinates of Equal Ordered Pairs Are Equal", Seq(Equals(OrderedPair(a, b), OrderedPair(c, d))), Equals(a, c))
  val secondCoordinatesOfEqualOrderedPairsAreEqual = createInference("Second Coordinates of Equal Ordered Pairs Are Equal", Seq(Equals(OrderedPair(a, b), OrderedPair(c, d))), Equals(b, d))

  // An equivalence whose extraction has the same shape as a reversal, which should not be used in place of one
  val equalityIsSymmetric = createInference("Equality Is Symmetric", Nil, Equivalence(Equals(a, b), Equals(b, a)))

  given availableEntries: AvailableEntries = defaultAvailableEntriesPlus(OrderedPair, firstCoordinatesOfEqualOrderedPairsAreEqual, secondCoordinatesOfEqualOrderedPairsAreEqual, equalityIsSymmetric)
  given variableDefinitions: VariableDefinitions = getVariableDefinitions(Nil, Seq(a -> 0, b -> 0, c -> 0, d -> 0))

  "known statements from premises" should {
    def getDerivation(premises: Seq[Statement], statement: Statement): Option[DerivedStatement] = {
      given stepContext: StepContext = createBaseStepContext(premises)
      implicitly[StepProvingContext].derivedStatementsFromPremisesBySerializedStatement.get(statement.serializedForHash)
    }
    def getDerivationInferenceNames(premises: Seq[Statement], statement: Statement): Option[Seq[String]] = {
      getDerivation(premises, statement).map(_.derivationSteps.map(_.inference.name))
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

    "record a reversal after a simplification in the derivation structure" in {
      getDerivation(Seq(pairEquality), Equals(c, a)) must beSome(beLike[DerivedStatement] {
        case DerivedStatement.Reversed(DerivedStatement.Simplified(DerivedStatement.Known(`pairEquality`), _), _) => ok
      })
    }

    "use a simplification as the primary inference of a reversed simplification" in {
      getDerivation(Seq(pairEquality), Equals(c, a)).flatMap(_.primaryInference) must beSome(firstCoordinatesOfEqualOrderedPairsAreEqual.summary)
      getDerivation(Seq(pairEquality), Equals(d, b)).flatMap(_.primaryInference) must beSome(secondCoordinatesOfEqualOrderedPairsAreEqual.summary)
    }

    "use a reversal as the primary inference of a premise that is only reversed" in {
      getDerivation(Seq(pairEquality), Equals(OrderedPair(c, d), OrderedPair(a, b))) must beSome(beLike[DerivedStatement] {
        case derivation @ DerivedStatement.Reversed(DerivedStatement.Known(`pairEquality`), _) => derivation.primaryInference must beSome(reverseEquality.summary)
      })
    }

    "display a rewrite using a reversed simplification as the simplification" in {
      val premiseDerivation = getDerivation(Seq(pairEquality), Equals(c, a)).get
      val substitutionStep = assertion(substitutionOfEquals, Seq(ElementOf($, d)), Seq(c, a))(SubstitutionContext.outsideProof)
      val rewriteStep = Step.RewriteStep(RewritePremise.Known(premiseDerivation), substitutionStep)
      GetDisplaySteps(rewriteStep, Seq(0)) must beLike[DisplayStep] {
        case DisplayStep.ElidedInference(_, inference, _, _) => inference must beEqualTo(firstCoordinatesOfEqualOrderedPairsAreEqual.summary)
      }
    }
  }
}
