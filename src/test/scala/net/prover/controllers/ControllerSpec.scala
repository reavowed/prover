package net.prover.controllers

import net.prover.controllers.models._
import net.prover.model.TestDefinitions._
import net.prover.model.definitions.CompoundStatementDefinition
import net.prover.model.expressions.{Statement, StatementVariable, Term, TermVariable}
import net.prover.model.proof._
import net.prover.model.{Inference, Substitutions, VariableDefinitions}
import net.prover.structure.EntryContext
import org.mockito.Mockito
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

  implicit class StepsConstructor(createSteps: SubstitutionContext => Seq[Step]) {
    def :+(other: SubstitutionContext => Step): SubstitutionContext => Seq[Step] = { sc =>
      createSteps(sc) :+ other(sc)
    }
  }
  implicit def seqConstructorToConstructorSeq(seq: Seq[SubstitutionContext => Step]): SubstitutionContext => Seq[Step] = { sc =>
    seq.map(_(sc))
  }

  def createOuterStepContextForStatements(
    statements: Seq[Statement],
    boundVariables: Seq[String])(
    implicit variableDefinitions: VariableDefinitions
  ): StepContext = {
    val baseContext = createBaseStepContext(Nil, statements)
    val contextWithBoundVariables = boundVariables.foldLeft(baseContext) { case (context, variable) => context.addBoundVariable(variable) }
    outerStepPath.foldLeft(contextWithBoundVariables) { case (context, index) => context.atIndex(index) }
  }
  def createOuterStepContextForSteps(
    steps: Seq[Step],
    boundVariables: Seq[String])(
    implicit variableDefinitions: VariableDefinitions
  ): StepContext = {
    createOuterStepContextForStatements(steps.mapCollect(_.provenStatement), boundVariables)
  }

  def definitionWithInference(
    inference: Inference,
    statements: Seq[Statement],
    terms: Seq[Term],
    extractionInferences: Seq[Inference],
    unwrappers: Seq[CompoundStatementDefinition] = Nil,
    premisesOption: Option[Seq[Statement]] = None,
    conclusionOption: Option[Statement] = None)(
    implicit entryContext: EntryContext
  ): StepDefinition = {
    val extraction = SubstatementExtractor.getInferenceExtractions(inference).find(_.extractionInferences == extractionInferences).get
    val substitutions = Substitutions(statements, terms)
    val serializedSubstitutions = SerializedSubstitutions(substitutions.statements.map(_.serialized), substitutions.terms.map(_.serialized))
    StepDefinition(
      Some(inference.id),
      None,
      serializedSubstitutions,
      extractionInferences.map(_.id),
      unwrappers.map(_.symbol),
      premisesOption.map(_.map(_.serialized)),
      conclusionOption.map(_.serialized),
      Some(extraction.additionalVariableNames))
  }
  def definitionWithPremise(
    premise: Statement,
    terms: Seq[Term],
    extractionInferences: Seq[Inference],
    conclusionOption: Option[Statement])(
    implicit entryContext: EntryContext,
    variableDefinitions: VariableDefinitions
  ): StepDefinition = {
    implicit val stepContext: StepContext = createOuterStepContextForStatements(Nil, Nil)
    val extraction = SubstatementExtractor.getPremiseExtractions(premise).find(_.extractionInferences == extractionInferences).get
    val substitutions = Substitutions(
      variableDefinitions.statements.mapWithIndex((d, i) => StatementVariable(i, (0 until d.arity).map($(_)))),
      variableDefinitions.terms.mapWithIndex((d, i) => TermVariable(i, (0 until d.arity).map($(_)))) ++ terms)
    val serializedSubstitutions = SerializedSubstitutions(substitutions.statements.map(_.serialized), substitutions.terms.map(_.serialized))
    StepDefinition(
      None,
      Some(premise.serialized),
      serializedSubstitutions,
      extractionInferences.map(_.id),
      Nil,
      None,
      conclusionOption.map(_.serialized),
      Some(extraction.additionalVariableNames))
  }

  def rewrite(
    inference: Inference,
    path: Seq[Int],
    extractionInferences: Seq[Inference]
  ): RewriteRequest = {
    RewriteRequest(path, Some(inference.id), None, extractionInferences.map(_.id))
  }
  def rewrite(
    premise: Statement,
    path: Seq[Int]
  ): RewriteRequest = {
    RewriteRequest(path, None, Some(premise.serialized), Nil)
  }

  def createService = {
    val service = mock[BookService]
    service
  }

  def mockReplaceStepsForInsertionAndReplacement(service: BookService): Unit = {
    service.replaceSteps[WithValue[InsertionAndReplacementProps]#Type](any, any, any, any, any)(any)(any) returns
      Success((
        ProofUpdateProps(
          MultipleStepReplacementProps(Nil, 0, 0, (0 until stepIndex + 1).map(i => Step.Target(StatementVariable(i)))),
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
          MultipleStepReplacementProps(Nil, 0, 0, (0 until stepIndex + 1).map(i => Step.Target(StatementVariable(i)))),
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
          MultipleStepReplacementProps(Nil, 0, 0, (0 until stepIndex + 1).map(i => Step.Target(StatementVariable(i)))),
          null,
          null),
        StepInsertionProps(outerStepPath :+ stepIndex, Nil)))
  }
  def mockReplaceStepsForSimpleReplacement(service: BookService): Unit = {
    service.replaceSteps[WithValue[Seq[Step]]#Type](any, any, any, any, any)(any)(any) returns Success((ProofUpdateProps(MultipleStepReplacementProps(Nil, 0, 0, (0 until stepIndex + 1).map(i => Step.Target(StatementVariable(i)))), null, null), Nil))
    Mockito.when(service.replaceStep[Step.Target](any, any, any, any, any)(any)(any)).thenCallRealMethod()
  }

  def eq[T](t: T) = org.mockito.Matchers.eq(t)

  def buildStepsWithReferences(stepsConstructor: SubstitutionContext => Seq[Step], boundVariables: Seq[String] = Nil)(implicit entryContext: EntryContext, variableDefinitions: VariableDefinitions): Seq[Step] = {
    implicit val provingContext = entryContextToProvingContext(entryContext)
    val steps = stepsConstructor(SubstitutionContext.withExtraParameters(boundVariables.length) (SubstitutionContext.outsideProof))
    val outerStepContext = createOuterStepContextForSteps(steps, boundVariables)
    steps.recalculateReferences(outerStepContext, provingContext)._1
  }
  def recalculateReferences(steps: Seq[Step], outerStepContext: StepContext)(implicit entryContext: EntryContext): Seq[Step] = {
    steps.recalculateReferences(outerStepContext, entryContextToProvingContext(entryContext))._1
  }

  def matchSteps(stepsConstructor: SubstitutionContext => Seq[Step], boundVariables: Seq[String] = Nil)(implicit entryContext: EntryContext, variableDefinitions: VariableDefinitions): Matcher[Seq[Step]] = {
    beEqualTo(buildStepsWithReferences(stepsConstructor, boundVariables)(entryContext, variableDefinitions))
  }

  def checkModifySteps(
    service: BookService,
    existingSteps: SubstitutionContext => Seq[Step],
    expectedSteps: SubstitutionContext => Seq[Step],
    boundVariables: Seq[String] = Nil)(
    implicit entryContext: EntryContext,
    variableDefinitions: VariableDefinitions
  ) = {
    there was one(service).replaceSteps[WithValue[InsertionAndReplacementProps]#Type](
      eq(bookKey),
      eq(chapterKey),
      eq(theoremKey),
      eq(proofIndex),
      eq(outerStepPath))(
      modifyStepsCallback(
        existingSteps,
        matchSteps(expectedSteps, boundVariables)(entryContext, variableDefinitions),
        boundVariables))(
      any)
  }
  def checkModifyStepsWithoutProps(
    service: BookService,
    existingSteps: SubstitutionContext => Seq[Step],
    expectedSteps: SubstitutionContext => Seq[Step],
    boundVariables: Seq[String] = Nil)(
    implicit entryContext: EntryContext,
    variableDefinitions: VariableDefinitions
  ) = {
    there was one(service).replaceSteps[WithValue[Seq[Step]]#Type](
      eq(bookKey),
      eq(chapterKey),
      eq(theoremKey),
      eq(proofIndex),
      eq(outerStepPath))(
      modifyStepsCallbackWithoutProps(
        existingSteps,
        matchSteps(expectedSteps, boundVariables)(entryContext, variableDefinitions),
        boundVariables))(
      any)
  }
  def checkModifyStepsWithMatcher(
    service: BookService,
    existingSteps: SubstitutionContext => Seq[Step],
    stepsMatcher: Matcher[Seq[Step]],
    boundVariables: Seq[String] = Nil)(
    implicit entryContext: EntryContext,
    variableDefinitions: VariableDefinitions
  ) = {
    there was one(service).replaceSteps[WithValue[InsertionAndReplacementProps]#Type](
      eq(bookKey),
      eq(chapterKey),
      eq(theoremKey),
      eq(proofIndex),
      eq(outerStepPath))(
      modifyStepsCallback(
        existingSteps,
        stepsMatcher,
        boundVariables))(
      any)
  }

  def modifyStepsCallbackWithoutProps(
    existingStepsFn: SubstitutionContext => Seq[Step],
    stepsMatcher: Matcher[Seq[Step]],
    boundVariables: Seq[String])(
    implicit entryContext: EntryContext,
    variableDefinitions: VariableDefinitions
  ): (Seq[Step], StepProvingContext) => Try[(Seq[Step], Seq[Step])] = {
    val existingSteps = existingStepsFn(SubstitutionContext.outsideProof)
    implicit val outerStepContext = createOuterStepContextForSteps(existingSteps, boundVariables)
    val existingStepsWithReferences = recalculateReferences(existingSteps, outerStepContext)
    (existingStepsWithReferences, implicitly[StepProvingContext]) -> beSuccessfulTry[(Seq[Step], Seq[Step])].withValue(stepsMatcher ^^ { t: (Seq[Step], Seq[Step]) => recalculateReferences(t._1, outerStepContext)(entryContext) })
  }
  def modifyStepsCallback(
    existingStepsFn: SubstitutionContext => Seq[Step],
    stepsMatcher: Matcher[Seq[Step]],
    boundVariables: Seq[String])(
    implicit entryContext: EntryContext,
    variableDefinitions: VariableDefinitions
  ): (Seq[Step], StepProvingContext) => Try[(Seq[Step], InsertionAndReplacementProps)] = {
    val existingSteps = buildStepsWithReferences(existingStepsFn, boundVariables)
    implicit val outerStepContext = createOuterStepContextForSteps(existingSteps, boundVariables)
    (existingSteps, implicitly[StepProvingContext]) -> beSuccessfulTry[(Seq[Step], InsertionAndReplacementProps)].withValue(stepsMatcher ^^ { t: (Seq[Step], InsertionAndReplacementProps) => recalculateReferences(t._1, outerStepContext)(entryContext) })
  }
}
