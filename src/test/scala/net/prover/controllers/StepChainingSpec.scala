package net.prover.controllers

import net.prover.controllers.StepChainingController.ChainedTargetDefinition
import net.prover.controllers.models.PathData
import net.prover.model.TestDefinitions
import net.prover.model.TestDefinitions._

class StepChainingSpec extends ControllerSpec {
  val lessThan = TestDefinitions.lessThan _ // prevent clash between this definition and the specs2 matcher of the same name

  implicit val availableEntries = defaultAvailableEntries
  implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 0, χ -> 0, ω -> 0), Seq(a -> 0, b -> 0))

  "adding a target" should {

    "add new chain correctly" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepChainingController(service)

      controller.addChainedTarget(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), ChainedTargetDefinition(ψ.serialized, Equivalence.symbol, Equivalence.symbol))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 1) :+ target(ψ) :+ target(Equivalence(φ, χ)),
          fillerSteps(stepIndex - 1) :+ target(ψ) :+
            target(Equivalence(φ, ψ)) :+
            target(Equivalence(ψ, χ)) :+
            assertion(equivalenceIsTransitive, Seq(φ, ψ, χ), Nil))
    }

    "add into existing chain correctly" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepChainingController(service)

      controller.addChainedTarget(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), ChainedTargetDefinition(χ.serialized, Equivalence.symbol, Equivalence.symbol))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 1) :+
          target(Equivalence(φ, ψ)) :+
          target(Equivalence(ψ, ω)) :+
          assertion(equivalenceIsTransitive, Seq(φ, ψ, ω), Nil),
        fillerSteps(stepIndex - 1) :+
          target(Equivalence(φ, ψ)) :+
          target(Equivalence(ψ, χ)) :+
          assertion(equivalenceIsTransitive, Seq(φ, ψ, χ), Nil) :+
          target(Equivalence(χ, ω)) :+
          assertion(equivalenceIsTransitive, Seq(φ, χ, ω), Nil))
    }
  }

  "adding a premise" should {
    "add left premise to new transitivity" in {
      val premise = Equivalence(φ, ψ)
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepChainingController(service)

      controller.addChainingFromLeft(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), definitionWithPremise(premise, Nil, Nil, None))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 1) :+ target(premise) :+ target(Equivalence(φ, χ)),
        fillerSteps(stepIndex - 1) :+ target(premise) :+ target(Equivalence(ψ, χ)) :+ assertion(equivalenceIsTransitive, Seq(φ, ψ, χ), Nil))
    }
    "add right premise to new transitivity" in {
      val premise = Equivalence(ψ, χ)
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepChainingController(service)

      controller.addChainingFromRight(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), definitionWithPremise(premise, Nil, Nil, None))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 1) :+ target(premise) :+ target(Equivalence(φ, χ)),
        fillerSteps(stepIndex - 1) :+ target(premise) :+ target(Equivalence(φ, ψ)) :+ assertion(equivalenceIsTransitive, Seq(φ, ψ, χ), Nil))
    }
  }

  "rewriting a component" should {

    "rewrite LHS using equality substitution" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController

      controller.rewriteLeft(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), Seq(Seq(rewrite(zeroIsRightIdentityForAddition, Nil, Nil))))

      checkModifySteps(
        service,
        fillerSteps(stepIndex) :+ target(lessThan(a, b)),
        fillerSteps(stepIndex) :+
          assertion(zeroIsRightIdentityForAddition, Nil, Seq(a)) :+
          target(lessThan(add(a, Zero), b)) :+
          elided(substitutionOfEquals, Seq(
            assertion(reverseEquality, Nil, Seq(a, add(a, Zero))),
            assertion(substitutionOfEquals, Seq(lessThan($, b)), Seq(add(a, Zero), a)))))
    }
    "rewrite RHS using equality substitution" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController

      controller.rewriteRight(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), Seq(Seq(rewrite(zeroIsRightIdentityForAddition, Nil, Nil))))

      checkModifySteps(
        service,
        fillerSteps(stepIndex) :+ target(lessThan(a, b)),
        fillerSteps(stepIndex) :+
          target(lessThan(a, add(b, Zero))) :+
          elided(zeroIsRightIdentityForAddition, Seq(
            assertion(zeroIsRightIdentityForAddition, Nil, Seq(b)),
            assertion(reverseEquality, Nil, Seq(b, add(b, Zero))))) :+
          assertion(substitutionOfEquals, Seq(lessThan(a, $)), Seq(add(b, Zero), b)))
    }

    "rewrite LHS using statement expansion" in {
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 1), Seq(a -> 0))
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController

      controller.rewriteLeft(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), Seq(Seq(rewrite(zeroIsRightIdentityForAddition, Seq(0), Nil))))

      checkModifySteps(
        service,
        fillerSteps(stepIndex) :+ target(Equivalence(ψ(a), φ)),
        fillerSteps(stepIndex) :+
          elided(zeroIsRightIdentityForAddition, Seq(
            assertion(zeroIsRightIdentityForAddition, Nil, Seq(a)),
            assertion(equivalenceOfSubstitutedEquals, Seq(ψ($)), Seq(a, add(a, Zero))))) :+
          target(Equivalence(ψ(add(a, Zero)), φ)) :+
          assertion(equivalenceIsTransitive, Seq(ψ(a), ψ(add(a, Zero)), φ), Nil))
    }

    "rewrite RHS using statement expansion" in {
      implicit val variableDefinitions = getVariableDefinitions(Seq(φ -> 0, ψ -> 1), Seq(a -> 0))
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController

      controller.rewriteRight(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), Seq(Seq(rewrite(zeroIsRightIdentityForAddition, Seq(0), Nil))))

      checkModifySteps(
        service,
        fillerSteps(stepIndex) :+ target(Equivalence(φ, ψ(a))),
        fillerSteps(stepIndex) :+
          target(Equivalence(φ, ψ(add(a, Zero)))) :+
          elided(zeroIsRightIdentityForAddition, Seq(
            elided(zeroIsRightIdentityForAddition, Seq(
              assertion(zeroIsRightIdentityForAddition, Nil, Seq(a)),
              assertion(reverseEquality, Nil, Seq(a, add(a, Zero))))),
            assertion(equivalenceOfSubstitutedEquals, Seq(ψ($)), Seq(add(a, Zero), a)))) :+
          assertion(equivalenceIsTransitive, Seq(φ, ψ(add(a, Zero)), ψ(a)), Nil))
    }

    "rewrite RHS of a non-transitive relation" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepRewriteController

      controller.rewriteRight(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), Seq(Seq(rewrite(zeroIsRightIdentityForAddition, Nil, Nil))))

      checkModifySteps(
        service,
        fillerSteps(stepIndex) :+ target(ElementOf(a, b)),
        fillerSteps(stepIndex) :+
          target(ElementOf(a, add(b, Zero))) :+
          elided(zeroIsRightIdentityForAddition, Seq(
            assertion(zeroIsRightIdentityForAddition, Nil, Seq(b)),
            assertion(reverseEquality, Nil, Seq(b, add(b, Zero))))) :+
          assertion(substitutionOfEquals, Seq(ElementOf(a, $)), Seq(add(b, Zero), b)))
    }
  }

  "proving with inference" should {
    "not add new RHS target if not necessary" in {
      implicit val service = mock[BookService]
      mockReplaceStepsForInsertionAndMultipleReplacement(service)
      val controller = new StepChainingController(service)

      controller.addChainingFromLeft(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definitionWithInference(zeroIsRightIdentityForAddition, Nil, Seq(b), Nil))

      checkModifySteps(
        service,
        fillerSteps(stepIndex - 1) :+
          target(Equals(a, b)) :+
          target(Equals(b, add(b, Zero))) :+
          assertion(equalityIsTransitive, Nil, Seq(a, b, add(b, Zero))),
        fillerSteps(stepIndex - 1) :+
          target(Equals(a, b)) :+
          assertion(zeroIsRightIdentityForAddition, Nil, Seq(b)) :+
          assertion(equalityIsTransitive, Nil, Seq(a, b, add(b, Zero))))
    }
  }
}
