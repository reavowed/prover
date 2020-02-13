package net.prover.controllers

import net.prover.controllers.models.{SerializedSubstitutions, StepDefinition}
import net.prover.model.{EntryContext, Inference}
import net.prover.model.TestDefinitions._
import net.prover.model.expressions.{Statement, StatementVariable, Term}
import net.prover.model.proof.{Step, StepContext, StepProvingContext, SubstatementExtractor, SubstitutionContext}
import org.specs2.matcher.{Matcher, ValueChecks}
import org.specs2.mock.mockito.{CalledMatchers, MockitoMatchers, MockitoStubs}
import org.specs2.mutable.Specification

import scala.util.{Success, Try}

trait ControllerSpec extends Specification with MockitoStubs with MockitoMatchers with CalledMatchers with ValueChecks {

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
    val extractionOption = SubstatementExtractor.getExtractionOptions(inference).find(_.extractionInferences == extractionInferences).get
    val substitutions = extractionOption.requiredSubstitutions.fill(statements, terms)
    val serializedSubstitutions = SerializedSubstitutions(substitutions.statements.mapValues(_.mapRight(_.serialized)), substitutions.terms.mapValues(_.mapRight(_.serialized)))
    StepDefinition(Some(inference.id), None, serializedSubstitutions, extractionInferences.map(_.id))
  }
  def definition(premise: Statement, terms: Seq[Term], extractionInferences: Seq[Inference]): StepDefinition = {
    implicit val stepContext = createOuterStepContext(Nil, Nil)
    val extractionOption = SubstatementExtractor.getExtractionOptions(premise).find(_.extractionInferences == extractionInferences).get
    val baseSubstitutions = premise.calculateSubstitutions(premise).get
    val substitutions = baseSubstitutions.copy(terms = baseSubstitutions.terms ++ extractionOption.requiredSubstitutions.fill(Nil, terms).terms)
    val serializedSubstitutions = SerializedSubstitutions(substitutions.statements.mapValues(_.mapRight(_.serialized)), substitutions.terms.mapValues(_.mapRight(_.serialized)))
    StepDefinition(None, Some(premise.serialized), serializedSubstitutions, extractionInferences.map(_.id))
  }

  def createService = {
    val service = mock[BookService]
    service.replaceSteps(any, any, any, any, any)(any) returns Success(null)
    service
  }

  def eq[T](t: T) = org.mockito.Matchers.eq(t)

  def checkModifySteps(
    service: BookService,
    existingSteps: Seq[Step],
    expectedSteps: Seq[Step])(
    implicit entryContext: EntryContext
  ) = {
    there was one(service).replaceSteps(eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(modifyStepsCallback(existingSteps, beEqualTo(expectedSteps))(entryContext))
  }
  def checkModifySteps(
    service: BookService,
    existingSteps: Seq[Step],
    stepsMatcher: Matcher[Seq[Step]])(
    implicit entryContext: EntryContext
  ) = {
    there was one(service).replaceSteps(eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(modifyStepsCallback(existingSteps, stepsMatcher)(entryContext))
  }

  def modifyStepsCallback(
    existingSteps: Seq[Step],
    stepsMatcher: Matcher[Seq[Step]])(
    implicit entryContext: EntryContext
  ): (Seq[Step], StepProvingContext) => Try[Seq[Step]] = {
    implicit val provingContext = entryContextToProvingContext(entryContext)
    val outerStepContext = createOuterStepContext(Nil, existingSteps.mapCollect(_.provenStatement).flatMap(_.requiredSubstitutions.terms).map(_._1).distinct)
    val existingStepsWithReferences = existingSteps.recalculateReferences(outerStepContext, provingContext)
    (existingStepsWithReferences, StepProvingContext(outerStepContext, provingContext)) -> beSuccessfulTry[Seq[Step]].withValue(stepsMatcher)
  }
}
