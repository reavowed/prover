package net.prover.controllers

import net.prover.controllers.models.{PathData, TheoremUpdateProps}
import net.prover.model.Inference
import org.specs2.mutable.Specification
import net.prover.model.TestDefinitions._
import net.prover.model.expressions.{Statement, StatementVariable, Term}
import net.prover.model.proof.{Step, StepContext, StepProvingContext}
import org.mockito.Matchers
import org.specs2.mock.Mockito

import scala.util.Success

class StepTransitivityControllerSpec extends Specification with Mockito {
  val bookKey = "test-book-key"
  val chapterKey = "test-chapter-key"
  val theoremKey = "test-theorem-key"
  val proofIndex = 3
  val outerStepPath = Seq(3, 1, 4, 1)
  val stepIndex = 5
  val stepPath = outerStepPath :+ stepIndex

  def assertion(inference: Inference, statements: Seq[Statement], terms: Seq[Term])(implicit stepContext: StepContext): Step.Assertion = {
    Step.Assertion.forInference(inference, inference.requiredSubstitutions.fill(statements, terms)).get
  }

  def fillerSteps(number: Int): Seq[Step] = (0 until number).map(i => Step.Target(StatementVariable(s"φ_$i")))

  def eq[T](t: T) = org.mockito.Matchers.eq(t)

  "adding a premise" should {
    "add left premise to new transitivity" in {
      val targetStatement = Equivalence(φ, χ)
      val premise = Equivalence(φ, ψ)

      val service = mock[BookService]
      service.modifySteps(any, any, any, any, any)(any) returns Success(null)
      val controller = new StepTransitivityController(service)

      controller.addPremiseLeft(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), premise.serialized)

      implicit val stepProvingContext = StepProvingContext(StepContext.withPremisesAndTerms(Seq(premise), Nil), implicitly)

      there was one(service).modifySteps(eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(
        (fillerSteps(stepIndex) :+ Step.Target(targetStatement), stepProvingContext) ->
          Success(fillerSteps(stepIndex) :+ Step.Target(Equivalence(ψ, χ)) :+ assertion(equivalenceIsTransitive, Seq(φ, ψ, χ), Nil)))
    }
    "add right premise to new transitivity" in {
      val targetStatement = Equivalence(φ, χ)
      val premise = Equivalence(ψ, χ)

      val service = mock[BookService]
      service.modifySteps(any, any, any, any, any)(any) returns Success(null)
      val controller = new StepTransitivityController(service)

      controller.addPremiseRight(bookKey, chapterKey, theoremKey, proofIndex, PathData(stepPath), premise.serialized)

      implicit val stepProvingContext = StepProvingContext(StepContext.withPremisesAndTerms(Seq(premise), Nil), implicitly)

      there was one(service).modifySteps(eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(
        (fillerSteps(stepIndex) :+ Step.Target(targetStatement), stepProvingContext) ->
          Success(fillerSteps(stepIndex) :+ Step.Target(Equivalence(φ, ψ)) :+ assertion(equivalenceIsTransitive, Seq(φ, ψ, χ), Nil)))
    }
  }
}
