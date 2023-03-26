package net.prover

import net.prover.controllers.BookService
import net.prover.controllers.models._
import net.prover.model.TestDefinitions._
import net.prover.model.expressions.StatementVariable
import net.prover.model.proof.{Step, StepContext, StepProvingContext, SubstitutionContext}
import net.prover.model.{EntryContext, VariableDefinitions}
import net.prover.util.FunctorTypes.WithValue
import org.mockito.Mockito
import org.specs2.SpecificationLike
import org.specs2.matcher.{MatchResult, Matcher}
import org.specs2.mock.mockito.{CalledMatchers, MockitoMatchers, MockitoStubs}

import scala.util.{Success, Try}

trait BookServiceHelper extends SpecificationLike with StepContextHelper with MockitoStubs with MockitoMatchers with CalledMatchers {
  private def eq[T](t: T) = org.mockito.Matchers.eq(t)

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

  def checkModifySteps(
    service: BookService,
    existingSteps: SubstitutionContext => Seq[Step],
    expectedSteps: SubstitutionContext => Seq[Step],
    boundVariables: Seq[String] = Nil)(
    implicit entryContext: EntryContext,
    variableDefinitions: VariableDefinitions
  ): MatchResult[Any] = {
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
  ): MatchResult[Any] = {
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
  ): MatchResult[Any] = {
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

  private def modifyStepsCallbackWithoutProps(
    existingStepsFn: SubstitutionContext => Seq[Step],
    stepsMatcher: Matcher[Seq[Step]],
    boundVariables: Seq[String])(
    implicit entryContext: EntryContext,
    variableDefinitions: VariableDefinitions
  ): (Seq[Step], StepProvingContext) => Try[(Seq[Step], Seq[Step])] = {
    val existingSteps = existingStepsFn(SubstitutionContext.outsideProof)
    implicit val outerStepContext = createOuterStepContext(boundVariables)
    val existingStepsWithReferences = recalculateReferences(existingSteps, outerStepContext)
    (existingStepsWithReferences, implicitly[StepProvingContext]) -> beSuccessfulTry[(Seq[Step], Seq[Step])].withValue(stepsMatcher ^^ { t: (Seq[Step], Seq[Step]) => recalculateReferences(t._1, outerStepContext)(entryContext) })
  }

  private def modifyStepsCallback(
    existingStepsFn: SubstitutionContext => Seq[Step],
    stepsMatcher: Matcher[Seq[Step]],
    boundVariables: Seq[String])(
    implicit entryContext: EntryContext,
    variableDefinitions: VariableDefinitions
  ): (Seq[Step], StepProvingContext) => Try[(Seq[Step], InsertionAndReplacementProps)] = {
    val existingSteps = buildStepsWithReferences(existingStepsFn, boundVariables)
    implicit val outerStepContext = createOuterStepContext(boundVariables)
    (existingSteps, implicitly[StepProvingContext]) -> beSuccessfulTry[(Seq[Step], InsertionAndReplacementProps)].withValue(stepsMatcher ^^ { t: (Seq[Step], InsertionAndReplacementProps) => recalculateReferences(t._1, outerStepContext)(entryContext) })
  }

  def matchSteps(stepsConstructor: SubstitutionContext => Seq[Step], boundVariables: Seq[String] = Nil)(implicit entryContext: EntryContext, variableDefinitions: VariableDefinitions): Matcher[Seq[Step]] = {
    beEqualTo(buildStepsWithReferences(stepsConstructor, boundVariables)(entryContext, variableDefinitions))
  }

  def recalculateReferences(steps: Seq[Step], outerStepContext: StepContext)(implicit entryContext: EntryContext): Seq[Step] = {
    steps.recalculateReferences(outerStepContext, entryContextToProvingContext(entryContext))._1
  }

  def buildStepsWithReferences(stepsConstructor: SubstitutionContext => Seq[Step], boundVariables: Seq[String] = Nil)(implicit entryContext: EntryContext, variableDefinitions: VariableDefinitions): Seq[Step] = {
    implicit val provingContext = entryContextToProvingContext(entryContext)
    val steps = stepsConstructor(SubstitutionContext.withExtraParameters(boundVariables.length)(SubstitutionContext.outsideProof))
    val outerStepContext = createOuterStepContext(boundVariables)
    steps.recalculateReferences(outerStepContext, provingContext)._1
  }

}
