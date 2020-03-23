package net.prover.controllers

import net.prover.controllers.models.{InsertionAndMultipleReplacementProps, InsertionAndReplacementProps, MultipleStepReplacementProps, ProofUpdateProps, RewriteRequest, SerializedSubstitutions, StepDefinition, StepInsertionProps, StepReplacementProps}
import net.prover.model.{EntryContext, Inference}
import net.prover.model.TestDefinitions._
import net.prover.model.entries.StatementDefinition
import net.prover.model.expressions.{Statement, StatementVariable, Term}
import net.prover.model.proof.{Step, StepContext, StepProvingContext, SubstatementExtractor, SubstitutionContext}
import org.mockito.Mockito
import org.specs2.matcher.{Matcher, ValueChecks}
import org.specs2.mock.mockito.{CalledMatchers, MockitoMatchers, MockitoStubs}
import org.specs2.mutable.Specification
import scalaz.Functor

import scala.util.{Success, Try}

trait ControllerSpec extends Specification with MockitoStubs with MockitoMatchers with CalledMatchers with ValueChecks {

  val bookKey = "test-book-key"
  val chapterKey = "test-chapter-key"
  val theoremKey = "test-theorem-key"
  val proofIndex = 3
  val outerStepPath = Seq(3, 1, 4, 1)
  val stepIndex = 5
  val stepPath = outerStepPath :+ stepIndex

  def assertion(inference: Inference, statements: Seq[Statement], terms: Seq[Term]): SubstitutionContext => Step.Assertion = { substitutionContext =>
    Step.Assertion.forInference(inference, inference.requiredSubstitutions.fill(statements, terms))(substitutionContext).get
  }
  def generalization(variableName: String, steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.Generalization = sc => Step.Generalization(variableName, steps(SubstitutionContext.withExtraParameter(sc)), ForAllDefinition)
  def deduction(antecedent: Statement, steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.Deduction = sc => Step.Deduction(antecedent, steps(sc), Implication)
  def target(statement: Statement): SubstitutionContext => Step.Target = _ => Step.Target(statement)
  def elided(inference: Inference, steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.Elided = sc => Step.Elided(steps(sc), Some(inference.summary), None)
  def elided(description: String, steps: SubstitutionContext => Seq[Step]): SubstitutionContext => Step.Elided = sc => Step.Elided(steps(sc), None, Some(description))

  def fillerSteps(number: Int): SubstitutionContext => Seq[Step] = _ => (0 until number).map(i => Step.Target(StatementVariable(s"φ_$i")))

  implicit class StepsConstructor(createSteps: SubstitutionContext => Seq[Step]) {
    def :+(other: SubstitutionContext => Step): SubstitutionContext => Seq[Step] = { sc =>
      createSteps(sc) :+ other(sc)
    }
  }
  implicit def seqConstructorToConstructorSeq(seq: Seq[SubstitutionContext => Step]): SubstitutionContext => Seq[Step] = { sc =>
    seq.map(_(sc))
  }
  def matchSteps(stepsConstructor: SubstitutionContext => Seq[Step], boundVariables: Seq[String] = Nil): Matcher[Seq[Step]] = beEqualTo(stepsConstructor(SubstitutionContext.withExtraParameters(boundVariables.length) (SubstitutionContext.outsideProof)))

  def createOuterStepContext(
    premises: Seq[Statement],
    termVariableNames: Seq[String],
    boundVariables: Seq[String] = Nil
  ) = {
    val baseContext = StepContext.withPremisesAndTerms(premises, termVariableNames)
    val contextWithBoundVariables = boundVariables.foldLeft(baseContext) { case (context, variable) => context.addBoundVariable(variable) }
    outerStepPath.foldLeft(contextWithBoundVariables) { case (context, index) => context.atIndex(index) }
  }

  def definitionWithInference(
    inference: Inference,
    statements: Seq[Statement],
    terms: Seq[Term],
    extractionInferences: Seq[Inference],
    unwrappers: Seq[StatementDefinition] = Nil,
    premisesOption: Option[Seq[Statement]] = None,
    conclusionOption: Option[Statement] = None
  ): StepDefinition = {
    val extractionOption = SubstatementExtractor.getExtractionOptions(inference).find(_.extractionInferences == extractionInferences).get
    val substitutions = extractionOption.requiredSubstitutions.fill(statements, terms)
    val serializedSubstitutions = SerializedSubstitutions(substitutions.statements.mapValues(_.mapRight(_.serialized)), substitutions.terms.mapValues(_.mapRight(_.serialized)))
    StepDefinition(
      Some(inference.id),
      None,
      serializedSubstitutions,
      extractionInferences.map(_.id),
      unwrappers.map(_.symbol),
      premisesOption.map(_.map(_.serialized)),
      conclusionOption.map(_.serialized),
      Some(extractionOption.additionalVariableNames))
  }
  def definitionWithPremise(
    premise: Statement,
    terms: Seq[Term],
    extractionInferences: Seq[Inference],
    conclusionOption: Option[Statement]
  ): StepDefinition = {
    implicit val stepContext = createOuterStepContext(Nil, Nil)
    val extractionOption = SubstatementExtractor.getExtractionOptions(premise).find(_.extractionInferences == extractionInferences).get
    val baseSubstitutions = premise.calculateSubstitutions(premise).get
    val substitutions = baseSubstitutions.copy(terms = baseSubstitutions.terms ++ extractionOption.requiredSubstitutions.fill(Nil, terms).terms)
    val serializedSubstitutions = SerializedSubstitutions(substitutions.statements.mapValues(_.mapRight(_.serialized)), substitutions.terms.mapValues(_.mapRight(_.serialized)))
    StepDefinition(
      None,
      Some(premise.serialized),
      serializedSubstitutions,
      extractionInferences.map(_.id),
      Nil,
      None,
      conclusionOption.map(_.serialized),
      Some(extractionOption.additionalVariableNames))
  }

  def rewrite(
    inference: Inference,
    path: Seq[Int],
    extractionInferences: Seq[Inference]
  ): RewriteRequest = {
    RewriteRequest(path, Some(inference.id), None, extractionInferences.map(_.id), false)
  }

  def createService = {
    val service = mock[BookService]
    service
  }

  def mockReplaceStepsForInsertionAndReplacement(service: BookService): Unit = {
    service.replaceSteps[WithValue[InsertionAndReplacementProps]#Type](any, any, any, any, any)(any)(any) returns
      Success((
        ProofUpdateProps(
          MultipleStepReplacementProps(Nil, 0, 0, (0 until stepIndex + 1).map(i => Step.Target(StatementVariable(s"φ_$i")))),
          null,
          null),
        InsertionAndReplacementProps(
          StepInsertionProps(outerStepPath :+ stepIndex, Nil),
          StepReplacementProps(outerStepPath :+ stepIndex, Nil))))
  }
  def mockReplaceStepsForInsertionAndMultipleReplacement(service: BookService): Unit = {
    service.replaceSteps[WithValue[InsertionAndMultipleReplacementProps]#Type](any, any, any, any, any)(any)(any) returns
      Success((
        ProofUpdateProps(
          MultipleStepReplacementProps(Nil, 0, 0, (0 until stepIndex + 1).map(i => Step.Target(StatementVariable(s"φ_$i")))),
          null,
          null),
        InsertionAndMultipleReplacementProps(
          StepInsertionProps(outerStepPath :+ stepIndex, Nil),
          MultipleStepReplacementProps(outerStepPath, stepIndex, stepIndex, Nil))))
  }
  def mockReplaceStepsForInsertion(service: BookService): Unit = {
    service.replaceSteps[WithValue[StepInsertionProps]#Type](any, any, any, any, any)(any)(any) returns
      Success((
        ProofUpdateProps(
          MultipleStepReplacementProps(Nil, 0, 0, (0 until stepIndex + 1).map(i => Step.Target(StatementVariable(s"φ_$i")))),
          null,
          null),
        StepInsertionProps(outerStepPath :+ stepIndex, Nil)))
  }
  def mockReplaceStepsForSimpleReplacement(service: BookService): Unit = {
    service.replaceSteps[WithValue[Seq[Step]]#Type](any, any, any, any, any)(any)(any) returns Success((ProofUpdateProps(MultipleStepReplacementProps(Nil, 0, 0, (0 until stepIndex + 1).map(i => Step.Target(StatementVariable(s"φ_$i")))), null, null), Nil))
    Mockito.when(service.replaceStep[Step.Target](any, any, any, any, any)(any)(any)).thenCallRealMethod()
  }

  def eq[T](t: T) = org.mockito.Matchers.eq(t)

  def checkModifySteps(
    service: BookService,
    existingSteps: SubstitutionContext => Seq[Step],
    expectedSteps: SubstitutionContext => Seq[Step],
    boundVariables: Seq[String] = Nil)(
    implicit entryContext: EntryContext
  ) = {
    there was one(service).replaceSteps[WithValue[InsertionAndReplacementProps]#Type](eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(modifyStepsCallback(existingSteps, matchSteps(expectedSteps, boundVariables), boundVariables)(entryContext))(any)
  }
  def checkModifyStepsWithoutProps(
    service: BookService,
    existingSteps: SubstitutionContext => Seq[Step],
    expectedSteps: SubstitutionContext => Seq[Step],
    boundVariables: Seq[String] = Nil)(
    implicit entryContext: EntryContext
  ) = {
    there was one(service).replaceSteps[WithValue[Seq[Step]]#Type](eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(modifyStepsCallbackWithoutProps(existingSteps, matchSteps(expectedSteps, boundVariables), boundVariables)(entryContext))(any)
  }
  def checkModifyStepsWithMatcher(
    service: BookService,
    existingSteps: SubstitutionContext => Seq[Step],
    stepsMatcher: Matcher[Seq[Step]],
    boundVariables: Seq[String] = Nil)(
    implicit entryContext: EntryContext
  ) = {
    there was one(service).replaceSteps[WithValue[InsertionAndReplacementProps]#Type](eq(bookKey), eq(chapterKey), eq(theoremKey), eq(proofIndex), eq(outerStepPath))(modifyStepsCallback(existingSteps, stepsMatcher, boundVariables)(entryContext))(any)
  }

  def modifyStepsCallbackWithoutProps(
    existingStepsFn: SubstitutionContext => Seq[Step],
    stepsMatcher: Matcher[Seq[Step]],
    boundVariables: Seq[String])(
    implicit entryContext: EntryContext
  ): (Seq[Step], StepProvingContext) => Try[(Seq[Step], Seq[Step])] = {
    implicit val provingContext = entryContextToProvingContext(entryContext)
    val existingSteps = existingStepsFn(SubstitutionContext.outsideProof)
    val outerStepContext = createOuterStepContext(Nil, existingSteps.mapCollect(_.provenStatement).flatMap(_.requiredSubstitutions.terms).map(_._1).distinct, boundVariables)
    val existingStepsWithReferences = existingSteps.recalculateReferences(outerStepContext, provingContext)._1
    (existingStepsWithReferences, StepProvingContext(outerStepContext, provingContext)) -> beSuccessfulTry[(Seq[Step], Seq[Step])].withValue(stepsMatcher ^^ { t: (Seq[Step], Seq[Step]) => t._1 })
  }
  def modifyStepsCallback(
    existingStepsFn: SubstitutionContext => Seq[Step],
    stepsMatcher: Matcher[Seq[Step]],
    boundVariables: Seq[String])(
    implicit entryContext: EntryContext
  ): (Seq[Step], StepProvingContext) => Try[(Seq[Step], InsertionAndReplacementProps)] = {
    implicit val provingContext = entryContextToProvingContext(entryContext)
    val existingSteps = existingStepsFn(SubstitutionContext.outsideProof)
    val outerStepContext = createOuterStepContext(Nil, existingSteps.mapCollect(_.provenStatement).flatMap(_.requiredSubstitutions.terms).map(_._1).distinct, boundVariables)
    val existingStepsWithReferences = existingSteps.recalculateReferences(outerStepContext, provingContext)._1
    (existingStepsWithReferences, StepProvingContext(outerStepContext, provingContext)) -> beSuccessfulTry[(Seq[Step], InsertionAndReplacementProps)].withValue(stepsMatcher ^^ { t: (Seq[Step], InsertionAndReplacementProps) => t._1 })
  }
}
