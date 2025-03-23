package net.prover.refactoring

import net.prover.StepBuilderHelper
import net.prover.model.TestDefinitions.*
import net.prover.model.expressions.DefinedStatement
import net.prover.model.proof.Step.AssertionStep
import net.prover.model.proof.{Step, StepContext, SubstitutionContext}
import net.prover.model.{AvailableEntries, TermVariablePlaceholder, VariableDefinitions}
import net.prover.theorems.RecalculateReferences
import org.specs2.mutable.Specification

class ReplaceElidedStepsSpec extends Specification with StepBuilderHelper {
  given availableEntries: AvailableEntries = defaultAvailableEntries

  "replace elided steps" should {
    "replace a wrapped assertion" in {
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 0), Nil)
      given outerStepContext: StepContext = createOuterStepContext(Nil)

      val initialSteps = recalculateReferences(Seq(
        target(ForAll("x")(Implication(φ($), ψ))),
        target(Negation(ψ)),
        elided(modusTollens, Seq(
          generalization("x", Seq(
            assertion(specification, Seq(Implication(φ($.^), ψ)), Seq($)),
            assertion(modusTollens, Seq(φ($), ψ), Nil)))))
      )(SubstitutionContext.outsideProof))
      val expectedSteps = recalculateReferences(Seq(
        target(ForAll("x")(Implication(φ($), ψ))),
        target(Negation(ψ)),
        wrappedInferenceApplication(Seq(
          generalization("x", Seq(
            assertion(specification, Seq(Implication(φ($.^), ψ)), Seq($)),
            assertion(modusTollens, Seq(φ($), ψ), Nil)))))
      )(SubstitutionContext.outsideProof))

      ReplaceElidedSteps(createStepsWithContext(initialSteps)) must beEqualTo(expectedSteps)
    }

    "replace an extraction" in {
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Nil, Seq(a -> 0, b -> 0))
      given outerStepContext: StepContext = createOuterStepContext(Nil)

      val initialSteps = recalculateReferences(Seq(
        target(Equals(a, b)),
        elided(membershipConditionForSingleton, Seq(
          assertion(membershipConditionForSingleton, Nil, Seq(b)),
          assertion(specification, Seq(Equivalence(ElementOf($, Singleton(b)), Equals($, b))), Seq(a)),
          assertion(reverseImplicationFromEquivalence, Seq(ElementOf(a, Singleton(b)), Equals(a, b)), Nil),
          assertion(modusPonens, Seq(Equals(a, b), ElementOf(a, Singleton(b))), Nil)))
      )(SubstitutionContext.outsideProof))
      val expectedSteps = recalculateReferences(Seq(
        target(Equals(a, b)),
        inferenceExtraction(
          assertion(membershipConditionForSingleton, Nil, Seq(b)),
          Seq(
            assertion(specification, Seq(Equivalence(ElementOf($, Singleton(b)), Equals($, b))), Seq(a)),
            assertion(reverseImplicationFromEquivalence, Seq(ElementOf(a, Singleton(b)), Equals(a, b)), Nil),
            assertion(modusPonens, Seq(Equals(a, b), ElementOf(a, Singleton(b))), Nil)))
      )(SubstitutionContext.outsideProof))

      ReplaceElidedSteps(createStepsWithContext(initialSteps)) must beEqualTo(expectedSteps)
    }

    "retain bound variable names when replacing an extraction" in {
      val f = TermVariablePlaceholder("f", 0)
      val x = TermVariablePlaceholder("x", 0)
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Nil, Seq(f -> 0, x-> 0))
      given outerStepContext: StepContext = createOuterStepContext(Nil)

      val initialSteps = recalculateReferences(Seq(
        target(Function(f)),
        target(ElementOf(x, Domain(f))),
        elided(Function.deconstructionInference, Seq(
          assertion(Function.deconstructionInference, Nil, Seq(f)),
          assertion(specification, Seq(Implication(ElementOf($, Domain(f)), ExistsUnique("y")(ElementOf(Pair($.^, $), f)))), Seq(x)),
          assertion(modusPonens, Seq(ElementOf(x, Domain(f)), ExistsUnique("y")(ElementOf(Pair(x, $), f))), Nil)))
      )(SubstitutionContext.outsideProof))
      val expectedSteps = recalculateReferences(Seq(
        target(Function(f)),
        target(ElementOf(x, Domain(f))),
        inferenceExtraction(
          assertion(Function.deconstructionInference, Nil, Seq(f)),
          Seq(
            assertion(specification, Seq(Implication(ElementOf($, Domain(f)), ExistsUnique("y")(ElementOf(Pair($.^, $), f)))), Seq(x)),
            assertion(modusPonens, Seq(ElementOf(x, Domain(f)), ExistsUnique("y")(ElementOf(Pair(x, $), f))), Nil)))
      )(SubstitutionContext.outsideProof))

      val actualSteps = ReplaceElidedSteps(createStepsWithContext(initialSteps))

      actualSteps must beEqualTo(expectedSteps)
      actualSteps.last.asInstanceOf[Step.InferenceExtractionStep]
        .inferenceExtraction.extraction.extractionSteps.last.statement.asInstanceOf[DefinedStatement]
        .boundVariableNames must beEqualTo(Seq("y"))
      actualSteps.last.asInstanceOf[Step.InferenceExtractionStep]
        .inferenceExtraction.extraction.extractionSteps.last.toProofStep.asInstanceOf[AssertionStep]
        .substitutions.statements.last.asInstanceOf[DefinedStatement]
        .boundVariableNames must beEqualTo(Seq("y"))
      actualSteps.last.asInstanceOf[Step.InferenceExtractionStep]
        .inferenceExtraction.extraction.extractionSteps.init.last.statement.asInstanceOf[DefinedStatement]
        .components.last.asInstanceOf[DefinedStatement]
        .boundVariableNames must beEqualTo(Seq("y"))
    }

    "replace a rewrite" in {
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 1), Seq(a -> 0, b -> 0))
      given outerStepContext: StepContext = createOuterStepContext(Nil)
        val initialSteps = recalculateReferences(Seq(
          target(ElementOf(a, Naturals)),
          target(ElementOf(b, Naturals)),
          elided(additionIsCommutative, Seq(
            assertion(additionIsCommutative, Nil, Seq(a, b)),
            assertion(equivalenceOfSubstitutedEquals, Seq(φ($)), Seq(add(a, b), add(b, a)))))
        )(SubstitutionContext.outsideProof))
        val expectedSteps = recalculateReferences(Seq(
          target(ElementOf(a, Naturals)),
          target(ElementOf(b, Naturals)),
          rewriteStep(
            Nil,
            assertion(additionIsCommutative, Nil, Seq(a, b)),
            assertion(equivalenceOfSubstitutedEquals, Seq(φ($)), Seq(add(a, b), add(b, a))))
        )(SubstitutionContext.outsideProof))

        ReplaceElidedSteps(createStepsWithContext(initialSteps)) must beEqualTo(expectedSteps)
    }

    "replace an existing statement extraction" in {
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 1), Seq(a -> 0))
      given outerStepContext: StepContext = createOuterStepContext(Nil)

      val mainPremise = ForAll("x")(Implication(φ($), ψ($)))
      val subsidiaryPremise = φ(a)

      val initialSteps = recalculateReferences(Seq(
        target(mainPremise),
        target(subsidiaryPremise),
        elided("Extraction", Seq(
          assertion(specification, Seq(Implication(φ($), ψ($))), Seq(a)),
          assertion(modusPonens, Seq(φ(a), ψ(a)), Nil)))
      )(SubstitutionContext.outsideProof))
      val expectedSteps = recalculateReferences(Seq(
        target(mainPremise),
        target(subsidiaryPremise),
        existingStatementExtraction(Seq(
          assertion(specification, Seq(Implication(φ($), ψ($))), Seq(a)),
          assertion(modusPonens, Seq(φ(a), ψ(a)), Nil)))
        )(SubstitutionContext.outsideProof))

      ReplaceElidedSteps(createStepsWithContext(initialSteps)) must beEqualTo(expectedSteps)
    }

    "replace an existing statement extraction with premises" in {
      given variableDefinitions: VariableDefinitions = getVariableDefinitions(Seq(φ -> 1, ψ -> 1), Seq(a -> 0))
      given outerStepContext: StepContext = createOuterStepContext(Nil)

      val mainPremise = ForAll("x")(Implication(Conjunction(φ($), ψ($)), χ($)))
      val firstSubsidiaryPremise = φ(a)
      val secondSubsidiaryPremise = ψ(a)

      val initialSteps = recalculateReferences(Seq(
        target(mainPremise),
        target(firstSubsidiaryPremise),
        target(secondSubsidiaryPremise),
        elided("Extraction", Seq(
          assertion(combineConjunction, Seq(firstSubsidiaryPremise, secondSubsidiaryPremise), Nil),
          elided("Extraction", Seq(
            assertion(specification, Seq(Implication(Conjunction(φ($), ψ($)), χ($))), Seq(a)),
            assertion(modusPonens, Seq(Conjunction(φ(a), ψ(a)), χ(a)), Nil)))))
      )(SubstitutionContext.outsideProof))
      val expectedSteps = recalculateReferences(Seq(
        target(mainPremise),
        target(firstSubsidiaryPremise),
        target(secondSubsidiaryPremise),
        existingStatementExtraction(
          Seq(known(Seq(assertion(combineConjunction, Seq(φ(a), ψ(a)), Nil)))),
          Seq(
            assertion(specification, Seq(Implication(Conjunction(φ($), ψ($)), χ($))), Seq(a)),
            assertion(modusPonens, Seq(Conjunction(φ(a), ψ(a)), χ(a)), Nil)))
      )(SubstitutionContext.outsideProof))

      ReplaceElidedSteps(createStepsWithContext(initialSteps)) must beEqualTo(expectedSteps)

    }
  }

}
