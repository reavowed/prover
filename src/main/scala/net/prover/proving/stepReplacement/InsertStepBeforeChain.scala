package net.prover.proving.stepReplacement

import net.prover.controllers.BookService
import net.prover.controllers.models.{PathData, ProofUpdateProps, StepInsertionProps}
import net.prover.exceptions.NotFoundException
import net.prover.model.proof.{Step, StepProvingContext}
import net.prover.util.FunctorTypes._

import scala.util.{Failure, Try}

object InsertStepBeforeChain {
  def apply(
    bookKey: String,
    chapterKey: String,
    theoremKey: String,
    proofIndex: Int,
    stepPath: PathData)(
    f: StepProvingContext => Try[Seq[Step]])(
    implicit bookService: BookService
  ): Try[ProofUpdateProps[StepInsertionProps]] = {
    stepPath.indexes match {
      case init :+ last =>
        bookService.replaceSteps[WithValue[StepInsertionProps]#Type](bookKey, chapterKey, theoremKey, proofIndex, init) { case (steps, stepProvingContext) =>
          steps.splitAtIndexIfValid(last).map { case (before, step, after) =>
            for {
              stepsToAddBeforeTransitive <- f(stepProvingContext.updateStepContext(_.addSteps(before).atIndex(last)))
            } yield AddTargetsBeforeChain(init, before, step +: after, stepsToAddBeforeTransitive)(stepProvingContext)
          }.orNotFound(s"Step $stepPath").flatten
        }.map { case (proofUpdateProps, stepInsertionProps) =>
          proofUpdateProps.withNewStepUpdateProps(stepInsertionProps.updateStepsFrom(proofUpdateProps.stepUpdates))
        }
      case _ =>
        Failure(NotFoundException(s"Step $stepPath"))
    }
  }
}
