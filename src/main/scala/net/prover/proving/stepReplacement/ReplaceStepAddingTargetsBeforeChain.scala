package net.prover.proving.stepReplacement

import net.prover.controllers.{BookService, OptionWithResponseExceptionOps, WithValue}
import net.prover.controllers.models.{InsertionAndReplacementProps, PathData, ProofUpdateProps, StepReplacementProps}
import net.prover.exceptions.NotFoundException
import net.prover.model.proof.{Step, StepProvingContext}

import scala.reflect.{ClassTag, classTag}
import scala.util.{Failure, Try}

object ReplaceStepAddingTargetsBeforeChain {
  def apply[TStep <: Step : ClassTag](
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData)(
    f: (TStep, StepProvingContext) => Try[(Step, Seq[Step])])(
    implicit bookService: BookService
  ): Try[ProofUpdateProps[InsertionAndReplacementProps]] = {
    stepPath.indexes match {
      case init :+ last =>
        bookService.replaceSteps[WithValue[InsertionAndReplacementProps]#Type](bookKey, chapterKey, theoremKey, proofIndex, init) { case (steps, stepProvingContext) =>
          steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
            for {
              typedStep <- step.asOptionalInstanceOf[TStep].orBadRequest(s"Step was not ${classTag[TStep].runtimeClass.getSimpleName}")
              (replacementStep, stepsToAddBeforeTransitive) <- f(typedStep, stepProvingContext.updateStepContext(_.addSteps(before).atIndex(last)))
              (newSteps, stepInsertionProps) = AddTargetsBeforeChain(init, before, replacementStep +: after, stepsToAddBeforeTransitive)(stepProvingContext)
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
