package net.prover.proving.stepReplacement

import net.prover.controllers.models.{InsertionAndReplacementProps, PathData, ProofUpdateProps, StepReplacementProps}
import net.prover.controllers.{BookService, OptionWithResponseExceptionOps}
import net.prover.entries.TypedStepWithContext
import net.prover.exceptions.NotFoundException
import net.prover.model.proof.Step
import net.prover.util.FunctorTypes._

import scala.reflect.{ClassTag, classTag}
import scala.util.{Failure, Try}

object ReplaceStepAddingTargetsBeforeChain {
  def apply[TStep <: Step : ClassTag](
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData)(
    f: TypedStepWithContext[TStep] => Try[(Step, Seq[Step])])(
    implicit bookService: BookService
  ): Try[ProofUpdateProps[InsertionAndReplacementProps]] = {
    stepPath.indexes match {
      case init :+ last =>
        bookService.replaceSteps[WithValue[InsertionAndReplacementProps]#Type](bookKey, chapterKey, theoremKey, proofIndex, init) { outerStepsWithContext =>
          outerStepsWithContext.steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
            val stepWithContext = outerStepsWithContext.atChild(before, step)
            for {
              typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
              (replacementStep, stepsToAddBeforeTransitive) <- f(stepWithContext.withStep(typedStep))
              (newSteps, stepInsertionProps) = AddTargetsBeforeChain(init, before, replacementStep +: after, stepsToAddBeforeTransitive)(outerStepsWithContext)
            } yield (newSteps, InsertionAndReplacementProps(stepInsertionProps, StepReplacementProps(stepPath.indexes, Seq(replacementStep))))
          }.orNotFound(s"Step $stepPath").flatten
        }.map { case (proofUpdateProps, stepProps) =>
          proofUpdateProps.withNewStepUpdateProps(
            InsertionAndReplacementProps(
              stepProps.insertion.updateStepsFrom(proofUpdateProps.stepUpdates),
              stepProps.replacement.updateStepsFrom(proofUpdateProps.stepUpdates, stepProps.insertion.newSteps.length)))
        }
      case _ =>
        Failure(NotFoundException(s"Step $stepPath"))
    }
  }
}
