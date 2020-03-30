package net.prover.controllers.models

import net.prover.model.entries.Theorem
import net.prover.model.proof.Step

case class TheoremUpdateProps(theorem: Theorem, newInferences: Map[String, InferenceSummary], stepsWithReferenceChanges: Seq[Seq[StepWithReferenceChange]])
case class ProofUpdateProps[T <: StepUpdateProps](stepUpdates: T, newInferences: Map[String, InferenceSummary], stepsWithReferenceChanges: Seq[StepWithReferenceChange]) {
  def withNewStepUpdateProps[S <: StepUpdateProps](newUpdateProps: S): ProofUpdateProps[S] = {
    ProofUpdateProps(newUpdateProps, newInferences, stepsWithReferenceChanges)
  }
}

case class StepWithReferenceChange(step: Step, path: Seq[Int])

sealed trait StepUpdateProps
case class StepInsertionProps(path: Seq[Int], newSteps: Seq[Step]) extends StepUpdateProps {
  def updateStepsFrom(multipleStepReplacementProps: MultipleStepReplacementProps): StepInsertionProps = {
    copy(newSteps = multipleStepReplacementProps.newSteps.slice(path.last - multipleStepReplacementProps.startIndex, path.last - multipleStepReplacementProps.startIndex + newSteps.length))
  }
}
case class StepDeletionProps(parentPath: Seq[Int], startIndex: Int, endIndex: Int) extends StepUpdateProps
case class StepReplacementProps(path: Seq[Int], newSteps: Seq[Step]) extends StepUpdateProps {
  def updateStepsFrom(multipleStepReplacementProps: MultipleStepReplacementProps, offset: Int): StepReplacementProps = {
    copy(newSteps = multipleStepReplacementProps.newSteps.slice(path.last + offset - multipleStepReplacementProps.startIndex, path.last + offset - multipleStepReplacementProps.startIndex + newSteps.length))
  }
}
case class MultipleStepReplacementProps(parentPath: Seq[Int], startIndex: Int, endIndex: Int, newSteps: Seq[Step]) extends StepUpdateProps {
  def updateStepsFrom(multipleStepReplacementProps: MultipleStepReplacementProps): MultipleStepReplacementProps = {
    copy(newSteps = multipleStepReplacementProps.newSteps.slice(startIndex - multipleStepReplacementProps.startIndex, startIndex - multipleStepReplacementProps.startIndex + newSteps.length))
  }
}
case class InsertionAndReplacementProps(insertion: StepInsertionProps, replacement: StepReplacementProps) extends StepUpdateProps
case class InsertionAndMultipleReplacementProps(insertion: StepInsertionProps, replacement: MultipleStepReplacementProps) extends StepUpdateProps
case class InsertionAndDeletionProps(insertion: StepInsertionProps, deletion: StepDeletionProps) extends StepUpdateProps
