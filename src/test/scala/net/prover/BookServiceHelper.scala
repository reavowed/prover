package net.prover

import net.prover.controllers.BookService
import net.prover.controllers.models.*
import net.prover.entries.StepsWithContext
import net.prover.model.expressions.StatementVariable
import net.prover.model.proof.{Step, StepContext, SubstitutionContext}
import net.prover.model.{AvailableEntries, VariableDefinitions}
import net.prover.util.FunctorTypes.WithValue
import org.mockito.Mockito
import org.specs2.SpecificationLike
import org.specs2.execute.Result
import org.specs2.matcher.Matcher

import scala.util.{Success, Try}

trait BookServiceHelper extends SpecificationLike with StepBuilderHelper with ContextHelper with MockitoHelpers {
  private def eq[T](t: T) = org.mockito.ArgumentMatchers.eq(t)

  def createService = {
    val service = mock[BookService]
    service
  }

  def mockReplaceStepsForInsertionAndReplacement(service: BookService): Unit = {
    service.replaceSteps[WithValue[InsertionAndReplacementProps]#Type](any, any, any, any, any)(any)(using any) returns
      Success((
        ProofUpdateProps(
          MultipleStepReplacementProps(Nil, 0, 0, (0 until stepIndex + 1).map(i => Step.TargetStep(StatementVariable(i)))),
          null,
          null),
        InsertionAndReplacementProps(
          StepInsertionProps(outerStepPath :+ stepIndex, Nil),
          StepReplacementProps(outerStepPath :+ stepIndex, Nil))))
  }

  def mockReplaceStepsForInsertionAndMultipleReplacement(service: BookService): Unit = {
    service.replaceSteps[WithValue[InsertionAndMultipleReplacementProps]#Type](any, any, any, any, any)(any)(using any) returns
      Success((
        ProofUpdateProps(
          MultipleStepReplacementProps(Nil, 0, 0, (0 until stepIndex + 1).map(i => Step.TargetStep(StatementVariable(i)))),
          null,
          null),
        InsertionAndMultipleReplacementProps(
          StepInsertionProps(outerStepPath :+ stepIndex, Nil),
          MultipleStepReplacementProps(outerStepPath, stepIndex, stepIndex, Nil))))
  }

  def mockReplaceStepsForInsertion(service: BookService): Unit = {
    service.replaceSteps[WithValue[StepInsertionProps]#Type](any, any, any, any, any)(any)(using any) returns
      Success((
        ProofUpdateProps(
          MultipleStepReplacementProps(Nil, 0, 0, (0 until stepIndex + 1).map(i => Step.TargetStep(StatementVariable(i)))),
          null,
          null),
        StepInsertionProps(outerStepPath :+ stepIndex, Nil)))
  }

  def mockReplaceStepsForSimpleReplacement(service: BookService): Unit = {
    service.replaceSteps[WithValue[Seq[Step]]#Type](any, any, any, any, any)(any)(using any) returns Success((ProofUpdateProps(MultipleStepReplacementProps(Nil, 0, 0, (0 until stepIndex + 1).map(i => Step.TargetStep(StatementVariable(i)))), null, null), Nil))
    Mockito.when(service.replaceStep[Step.TargetStep](any, any, any, any, any)(any)(using any)).thenCallRealMethod()
  }

  def checkModifySteps(
    service: BookService,
    existingStepsConstructor: SubstitutionContext => Seq[Step],
    expectedStepsConstructor: SubstitutionContext => Seq[Step],
    boundVariables: Seq[String] = Nil)(
    using availableEntries: AvailableEntries,
    variableDefinitions: VariableDefinitions
  ): Result = {
    val existingSteps = createStepsWithContext(existingStepsConstructor, boundVariables)
    there was one(service).replaceSteps[WithValue[InsertionAndReplacementProps]#Type](
      eq(bookKey),
      eq(chapterKey),
      eq(theoremKey),
      eq(proofIndex),
      eq(outerStepPath))(
      modifyStepsCallback(
        existingSteps,
        matchSteps(
          expectedStepsConstructor,
          boundVariables)(
          using availableEntries,
          variableDefinitions),
        boundVariables))(
      using any)
  }

  def checkModifyStepsWithoutProps(
    service: BookService,
    existingStepsConstructor: SubstitutionContext => Seq[Step],
    expectedStepsConstructor: SubstitutionContext => Seq[Step],
    boundVariables: Seq[String] = Nil)(
    using availableEntries: AvailableEntries,
    variableDefinitions: VariableDefinitions
  ): Result = {
    val existingSteps = createStepsWithContext(existingStepsConstructor, boundVariables)
    there was one(service).replaceSteps[WithValue[Seq[Step]]#Type](
      eq(bookKey),
      eq(chapterKey),
      eq(theoremKey),
      eq(proofIndex),
      eq(outerStepPath))(
      modifyStepsCallbackWithoutProps(
        existingSteps,
        matchSteps(
          expectedStepsConstructor,
          boundVariables)(
          using availableEntries,
          variableDefinitions),
        boundVariables))(
      using any)
  }

  def checkModifyStepsWithMatcher(
    service: BookService,
    existingStepsConstructor: SubstitutionContext => Seq[Step],
    stepsMatcher: Matcher[Seq[Step]],
    boundVariables: Seq[String] = Nil)(
    using availableEntries: AvailableEntries,
    variableDefinitions: VariableDefinitions
  ): Result = {
    val existingSteps = createStepsWithContext(existingStepsConstructor, boundVariables)
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
      using any)
  }

  private def createStepsWithContext(stepsConstructor: SubstitutionContext => Seq[Step], boundVariables: Seq[String] = Nil)(using availableEntries: AvailableEntries, variableDefinitions: VariableDefinitions): StepsWithContext = {
    val steps = createSteps(stepsConstructor, boundVariables)
    given outerStepContext: StepContext = createOuterStepContext(boundVariables)
    createStepsWithContext(steps)
  }

  private def modifyStepsCallbackWithoutProps(
    existingSteps: StepsWithContext,
    stepsMatcher: Matcher[Seq[Step]],
    boundVariables: Seq[String])(
    using availableEntries: AvailableEntries,
    variableDefinitions: VariableDefinitions
  ): StepsWithContext => Try[(Seq[Step], Seq[Step])] = {
    given outerStepContext: StepContext = createOuterStepContext(boundVariables)
    existingSteps -> beSuccessfulTry[(Seq[Step], Seq[Step])].withValue((stepsMatcher and beStepsThatMakeValidTheorem(boundVariables)) ^^ { (t: (Seq[Step], Seq[Step])) => recalculateReferences(t._1) })
  }

  private def modifyStepsCallback(
    existingSteps: StepsWithContext,
    stepsMatcher: Matcher[Seq[Step]],
    boundVariables: Seq[String])(
    using availableEntries: AvailableEntries,
    variableDefinitions: VariableDefinitions
  ): StepsWithContext => Try[(Seq[Step], InsertionAndReplacementProps)] = {
    given outerStepContext: StepContext = createOuterStepContext(boundVariables)
    existingSteps -> beSuccessfulTry[(Seq[Step], InsertionAndReplacementProps)]
      .withValue(
        (stepsMatcher and beStepsThatMakeValidTheorem(boundVariables)) ^^
          { (t: (Seq[Step], InsertionAndReplacementProps)) => recalculateReferences(t._1) })
  }

  def matchSteps(
    stepsConstructor: SubstitutionContext => Seq[Step],
    boundVariables: Seq[String] = Nil)(
    using availableEntries: AvailableEntries,
    variableDefinitions: VariableDefinitions
  ): Matcher[Seq[Step]] = {
    beEqualTo(createSteps(stepsConstructor, boundVariables)(availableEntries, variableDefinitions))
  }
}
