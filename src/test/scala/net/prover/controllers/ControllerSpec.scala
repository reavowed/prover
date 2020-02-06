package net.prover.controllers

import net.prover.controllers.models.{SerializedSubstitutions, StepDefinition}
import net.prover.model.Inference
import net.prover.model.expressions.{Statement, StatementVariable, Term}
import net.prover.model.proof.{Step, StepContext, StepProvingContext, SubstitutionContext}
import org.specs2.mock.mockito.{CalledMatchers, MockitoMatchers, MockitoStubs}
import org.specs2.mutable.Specification

import scala.util.{Success, Try}
import net.prover.model.TestDefinitions._

trait ControllerSpec extends Specification with MockitoStubs with MockitoMatchers with CalledMatchers {

  val bookKey = "test-book-key"
  val chapterKey = "test-chapter-key"
  val theoremKey = "test-theorem-key"
  val proofIndex = 3
  val outerStepPath = Seq(3, 1, 4, 1)
  val stepIndex = 5
  val stepPath = outerStepPath :+ stepIndex

  def assertion(inference: Inference, statements: Seq[Statement], terms: Seq[Term]): Step.Assertion = {
    implicit def substitutionContext = SubstitutionContext.outsideProof
    Step.Assertion.forInference(inference, inference.requiredSubstitutions.fill(statements, terms)).get
  }
  def target(statement: Statement): Step.Target = Step.Target(statement)
  def elided(inference: Inference, steps: Seq[Step]) = Step.Elided(steps, Some(inference.summary), None)
  def elided(description: String, steps: Seq[Step]) = Step.Elided(steps, None, Some(description))
  def fillerSteps(number: Int): Seq[Step] = (0 until number).map(i => Step.Target(StatementVariable(s"φ_$i")))

  def createOuterStepContext(premises: Seq[Statement], termVariableNames: Seq[String]) = outerStepPath.foldLeft(StepContext.withPremisesAndTerms(premises, termVariableNames)) { case (context, index) => context.atIndex(index)}

  def definition(inference: Inference, statements: Seq[Statement], terms: Seq[Term], extractionInferences: Seq[Inference]): StepDefinition = {
    val substitutions = inference.requiredSubstitutions.fill(statements, terms)
    val serializedSubstitutions = SerializedSubstitutions(substitutions.statements.mapValues(_.mapRight(_.serialized)), substitutions.terms.mapValues(_.mapRight(_.serialized)))
    StepDefinition(Some(inference.id), None, serializedSubstitutions, extractionInferences.map(_.id))
  }
  def definition(premise: Statement, terms: Map[String, Term], extractionInferences: Seq[Inference]): StepDefinition = {
    val serializedSubstitutions = SerializedSubstitutions(Map.empty, terms.mapValues(t => 0 -> t.serialized))
    StepDefinition(None, Some(premise.serialized), serializedSubstitutions, extractionInferences.map(_.id))
  }

  def createService = {
    val service = mock[BookService]
    service.modifySteps(any, any, any, any, any)(any) returns Success(null)
    service
  }

  def eq[T](t: T) = org.mockito.Matchers.eq(t)

  def checkModifySteps(
    service: BookService,
    existingSteps: Seq[Step],
    expectedSteps: Seq[Step]
  ) = {
    there was one(service).modifySteps(eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(modifyStepsCallback(existingSteps, expectedSteps))
  }

  def modifyStepsCallback(existingSteps: Seq[Step], expectedSteps: Seq[Step]): (Seq[Step], StepProvingContext) => Try[Seq[Step]] = {
    val outerStepContext = createOuterStepContext(Nil, existingSteps.mapCollect(_.provenStatement).flatMap(_.requiredSubstitutions.terms).map(_._1).distinct)
    val existingStepsWithReferences = existingSteps.recalculateReferences(outerStepContext, implicitly)
    (existingStepsWithReferences, StepProvingContext(outerStepContext, implicitly)) -> Success(expectedSteps)
  }

}
