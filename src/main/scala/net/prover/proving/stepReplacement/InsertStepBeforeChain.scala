package net.prover.proving.stepReplacement

import net.prover.controllers.BookService
import net.prover.controllers.models.{PathData, ProofUpdateProps, StepInsertionProps}
import net.prover.entries.StepWithContext
import net.prover.exceptions.NotFoundException
import net.prover.model.proof.Step
import net.prover.util.FunctorTypes._

import scala.util.{Failure, Try}

object InsertStepBeforeChain {
  def apply(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData)(
    f: StepWithContext => Try[Seq[Step]])(
    implicit bookService: BookService
  ): Try[ProofUpdateProps[StepInsertionProps]] = {
    stepPath.indexes match {
      case init :+ last =>
        bookService.replaceSteps[WithValue[StepInsertionProps]#Type](bookKey, chapterKey, theoremKey, proofIndex, init) { outerStepsWithContext =>
          outerStepsWithContext.steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
            for {
              stepsToAddBeforeTransitive <- f(outerStepsWithContext.atChild(before, step))
            } yield AddTargetsBeforeChain(init, before, step +: after, stepsToAddBeforeTransitive)(outerStepsWithContext)
          }.orNotFound(s"Step $stepPath").flatten
        }.map { case (proofUpdateProps, stepInsertionProps) =>
          proofUpdateProps.withNewStepUpdateProps(stepInsertionProps.updateStepsFrom(proofUpdateProps.stepUpdates))
        }
      case _ =>
        Failure(NotFoundException(s"Step $stepPath"))
    }
  }
}
