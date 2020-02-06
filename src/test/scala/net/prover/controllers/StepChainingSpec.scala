package net.prover.controllers

import net.prover.controllers.models.{PathData, RewriteRequest, SerializedSubstitutions, StepDefinition}
import net.prover.model.TestDefinitions._
import net.prover.model.expressions.{Statement, StatementVariable, Term}
import net.prover.model.proof.{Premise, Step, StepContext, StepProvingContext, StepReference, SubstitutionContext}
import net.prover.model.{Inference, TestDefinitions}
import org.specs2.mock.mockito.{CalledMatchers, MockitoMatchers, MockitoStubs}
import org.specs2.mutable.Specification

import scala.util.{Success, Try}

class StepChainingSpec extends Specification with MockitoStubs with MockitoMatchers with CalledMatchers {
  val lessThan = TestDefinitions.lessThan _ // prevent clash between this definition and the specs2 matcher of the same name

  val bookKey = "test-book-key"
  val chapterKey = "test-chapter-key"
  val theoremKey = "test-theorem-key"
  val proofIndex = 3
  val outerStepPath = Seq(3, 1, 4, 1)
  val stepIndex = 5
  val stepPath = outerStepPath :+ stepIndex

  implicit def substitutionContext = SubstitutionContext.outsideProof

  def assertion(inference: Inference, statements: Seq[Statement], terms: Seq[Term]): Step.Assertion = {
    Step.Assertion.forInference(inference, inference.requiredSubstitutions.fill(statements, terms)).get
  }
  def target(statement: Statement): Step.Target = Step.Target(statement)
  def elided(inference: Inference, steps: Seq[Step]) = Step.Elided(steps, Some(inference.summary), None)

  def definition(inference: Inference, statements: Seq[Statement], terms: Seq[Term], extractionInferences: Seq[Inference]): StepDefinition = {
    val substitutions = inference.requiredSubstitutions.fill(statements, terms)
    val serializedSubstitutions = SerializedSubstitutions(substitutions.statements.mapValues(_.mapRight(_.serialized)), substitutions.terms.mapValues(_.mapRight(_.serialized)))
    StepDefinition(Some(inference.id), None, serializedSubstitutions, extractionInferences.map(_.id))
  }

  def fillerSteps(number: Int): Seq[Step] = (0 until number).map(i => Step.Target(StatementVariable(s"φ_$i")))

  def eq[T](t: T) = org.mockito.Matchers.eq(t)

  def createOuterStepContext(premises: Seq[Statement], terms: Seq[TermVariablePlaceholder]) = outerStepPath.foldLeft(StepContext.withPremisesAndTerms(premises, terms.map(_.name))) { case (context, index) => context.atIndex(index)}
  def outerStepContext = createOuterStepContext(Nil, Nil)
  def outerStepProvingContext = StepProvingContext(outerStepContext, implicitly)

  def modifyStepsCallback(
    existingSteps: Seq[Step],
    expectedSteps: Seq[Step])(
    implicit outerStepContext: StepContext
  ): (Seq[Step], StepProvingContext) => Try[Seq[Step]] = {
    val existingStepsWithReferences = existingSteps.recalculateReferences(outerStepContext, implicitly)
    (existingStepsWithReferences, StepProvingContext(outerStepContext, implicitly)) -> Success(expectedSteps)
  }

  "adding a target" should {
    "add new chain correctly" in {
      val service = mock[BookService]
      service.modifySteps(any, any, any, any, any)(any) returns Success(null)
      val controller = new StepCreationController(service)
      controller.addTransitiveTarget(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), ψ.serialized)

      implicit val stepContext = outerStepContext

      there was one(service).modifySteps(eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(
        modifyStepsCallback(
          fillerSteps(stepIndex) :+ target(Equivalence(φ, χ)),
            fillerSteps(stepIndex) :+
              target(Equivalence(φ, ψ)) :+
              target(Equivalence(ψ, χ)) :+
              assertion(equivalenceIsTransitive, Seq(φ, ψ, χ), Nil)))
    }

    "add into existing chain correctly" in {
      val service = mock[BookService]
      service.modifySteps(any, any, any, any, any)(any) returns Success(null)
      val controller = new StepCreationController(service)
      controller.addTransitiveTarget(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), χ.serialized)

      implicit val stepContext = outerStepContext

      there was one(service).modifySteps(eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(
        modifyStepsCallback(
          fillerSteps(stepIndex - 1) :+
            target(Equivalence(φ, ψ)) :+
            target(Equivalence(ψ, ω)) :+
            assertion(equivalenceIsTransitive, Seq(φ, ψ, ω), Nil),
          fillerSteps(stepIndex - 1) :+
            target(Equivalence(φ, ψ)) :+
            target(Equivalence(ψ, χ)) :+
            assertion(equivalenceIsTransitive, Seq(φ, ψ, χ), Nil) :+
            target(Equivalence(χ, ω)) :+
            assertion(equivalenceIsTransitive, Seq(φ, χ, ω), Nil)))
    }
  }

  "adding a premise" should {
    "add left premise to new transitivity" in {
      val targetStatement = Equivalence(φ, χ)
      val premise = Equivalence(φ, ψ)

      val service = mock[BookService]
      service.modifySteps(any, any, any, any, any)(any) returns Success(null)
      val controller = new StepTransitivityController(service)

      controller.addPremiseLeft(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), premise.serialized)

      implicit val stepContext = createOuterStepContext(Seq(premise), Nil)

      there was one(service).modifySteps(eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(
        modifyStepsCallback(
          fillerSteps(stepIndex) :+ target(targetStatement),
          fillerSteps(stepIndex) :+ target(Equivalence(ψ, χ)) :+ assertion(equivalenceIsTransitive, Seq(φ, ψ, χ), Nil)))
    }
    "add right premise to new transitivity" in {
      val targetStatement = Equivalence(φ, χ)
      val premise = Equivalence(ψ, χ)

      val service = mock[BookService]
      service.modifySteps(any, any, any, any, any)(any) returns Success(null)
      val controller = new StepTransitivityController(service)

      controller.addPremiseRight(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), premise.serialized)

      implicit val stepContext = createOuterStepContext(Seq(premise), Nil)

      there was one(service).modifySteps(eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(
        modifyStepsCallback(
          fillerSteps(stepIndex) :+ target(targetStatement),
          fillerSteps(stepIndex) :+ target(Equivalence(φ, ψ)) :+ assertion(equivalenceIsTransitive, Seq(φ, ψ, χ), Nil)))
    }
  }

  "rewriting a component" should {
    "rewrite LHS using equality substitution" in {
      val targetStatement = lessThan(a, b)

      val service = mock[BookService]
      service.modifySteps(any, any, any, any, any)(any) returns Success(null)
      val controller = new StepRewriteController(service)

      controller.rewriteLeft(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), Seq(Seq(RewriteRequest(Nil, Some(addingZeroIsSame.id), None, false))))

      implicit val stepContext = createOuterStepContext(Nil, Nil)

      there was one(service).modifySteps(eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(
        modifyStepsCallback(
          fillerSteps(stepIndex) :+ target(targetStatement),
          fillerSteps(stepIndex) :+
            assertion(addingZeroIsSame, Nil, Seq(a)) :+
            target(lessThan(add(a, Zero), b)) :+
            elided(substitutionOfEquals, Seq(
              assertion(reverseEquality, Nil, Seq(a, add(a, Zero))),
              assertion(substitutionOfEquals, Seq(lessThan($, b)), Seq(add(a, Zero), a))))))
    }
    "rewrite RHS using equality substitution" in {
      val targetStatement = lessThan(a, b)

      val service = mock[BookService]
      service.modifySteps(any, any, any, any, any)(any) returns Success(null)
      val controller = new StepRewriteController(service)

      controller.rewriteRight(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), Seq(Seq(RewriteRequest(Nil, Some(addingZeroIsSame.id), None, false))))

      implicit val stepContext = createOuterStepContext(Nil, Nil)

      there was one(service).modifySteps(eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(
        modifyStepsCallback(
          fillerSteps(stepIndex) :+ target(targetStatement),
          fillerSteps(stepIndex) :+
            target(lessThan(a, add(b, Zero))) :+
            elided(addingZeroIsSame, Seq(
              assertion(addingZeroIsSame, Nil, Seq(b)),
              assertion(reverseEquality, Nil, Seq(b, add(b, Zero))))) :+
            assertion(substitutionOfEquals, Seq(lessThan(a, $)), Seq(add(b, Zero), b))))
    }

    "rewrite LHS using statement expansion" in {
      val targetStatement = Equivalence(φ(a), ψ)

      val service = mock[BookService]
      service.modifySteps(any, any, any, any, any)(any) returns Success(null)
      val controller = new StepRewriteController(service)

      controller.rewriteLeft(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), Seq(Seq(RewriteRequest(Seq(0), Some(addingZeroIsSame.id), None, false))))

      implicit val stepContext = StepContext.withPremisesAndTerms(Nil, Nil)

      there was one(service).modifySteps(eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(
        modifyStepsCallback(
          fillerSteps(stepIndex) :+ target(targetStatement),
          fillerSteps(stepIndex) :+
            elided(addingZeroIsSame, Seq(
              assertion(addingZeroIsSame, Nil, Seq(a)),
              assertion(equivalenceOfSubstitutedEquals, Seq(φ($)), Seq(a, add(a, Zero))))) :+
            target(Equivalence(φ(add(a, Zero)), ψ)) :+
            assertion(equivalenceIsTransitive, Seq(φ(a), φ(add(a, Zero)), ψ), Nil)))
    }

    "rewrite RHS using statement expansion" in {
      val targetStatement = Equivalence(φ, ψ(a))

      val service = mock[BookService]
      service.modifySteps(any, any, any, any, any)(any) returns Success(null)
      val controller = new StepRewriteController(service)

      controller.rewriteRight(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), Seq(Seq(RewriteRequest(Seq(0), Some(addingZeroIsSame.id), None, false))))

      implicit val stepContext = StepContext.withPremisesAndTerms(Nil, Nil)

      there was one(service).modifySteps(eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(
        modifyStepsCallback(
          fillerSteps(stepIndex) :+ target(targetStatement),
          fillerSteps(stepIndex) :+
            target(Equivalence(φ, ψ(add(a, Zero)))) :+
            elided(addingZeroIsSame, Seq(
              assertion(addingZeroIsSame, Nil, Seq(a)),
              assertion(reverseEquality, Nil, Seq(a, add(a, Zero))),
              assertion(equivalenceOfSubstitutedEquals, Seq(ψ($)), Seq(add(a, Zero), a)))) :+
            assertion(equivalenceIsTransitive, Seq(φ, ψ(add(a, Zero)), ψ(a)), Nil)))
    }
  }

  "proving with inference" should {
    "not add new RHS target if not necessary" in {
      val service = mock[BookService]
      service.modifySteps(any, any, any, any, any)(any) returns Success(null)
      val controller = new StepCreationController(service)
      controller.addTransitivityFromLeft(
        bookKey,
        chapterKey,
        theoremKey,
        proofIndex,
        PathData(stepPath),
        definition(addingZeroIsSame, Nil, Seq(b), Nil))

      implicit val stepContext = StepContext.withPremisesAndTerms(Nil, Seq(a, b))

      there was one(service).modifySteps(eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(
        modifyStepsCallback(
          fillerSteps(stepIndex - 1) :+
            target(Equals(a, b)) :+
            target(Equals(b, add(b, Zero))) :+
            assertion(equalityIsTransitive, Nil, Seq(a, b, add(b, Zero))),
          fillerSteps(stepIndex - 1) :+
            target(Equals(a, b)) :+
            assertion(addingZeroIsSame, Nil, Seq(b)) :+
            assertion(equalityIsTransitive, Nil, Seq(a, b, add(b, Zero)))))
    }
  }
}
