package net.prover.controllers.models

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import net.prover.entries.TheoremWithContext
import net.prover.model.entries.Theorem
import net.prover.model.proof.Step
import net.prover.theorems.{DisplayStep, GetDisplaySteps}

@JsonIgnoreProperties(Array("theoremWithContext"))
case class TheoremUpdateProps(
  theorem: Theorem,
  theoremWithContext: TheoremWithContext,
  newInferences: Map[String, InferenceSummary],
  stepsWithReferenceChanges: Seq[List[StepWithReferenceChange]])

case class ProofUpdateProps[T <: StepUpdateProps](
  stepUpdates: T,
  newInferences: Map[String, InferenceSummary],
  stepsWithReferenceChanges: Seq[StepWithReferenceChange]
) {
  def withNewStepUpdateProps[S <: StepUpdateProps](newUpdateProps: S): ProofUpdateProps[S] = {
    ProofUpdateProps(newUpdateProps, newInferences, stepsWithReferenceChanges)
  }
  def getStepsWithReferenceChanges: Seq[DisplayStep] = {
    stepsWithReferenceChanges.map {
      case StepWithReferenceChange(step, path) => GetDisplaySteps(step, path)
    }
  }
}

case class StepWithReferenceChange(step: Step, path: Seq[Int])

sealed trait StepUpdateProps
case class StepInsertionProps(path: Seq[Int], newSteps: Seq[Step]) extends StepUpdateProps {
  def updateStepsFrom(multipleStepReplacementProps: MultipleStepReplacementProps): StepInsertionProps = {
    copy(newSteps = multipleStepReplacementProps.newSteps.slice(path.last - multipleStepReplacementProps.startIndex, path.last - multipleStepReplacementProps.startIndex + newSteps.length))
  }
  def getNewSteps: Seq[DisplayStep] = GetDisplaySteps(newSteps, path.init, path.last)
}
case class StepDeletionProps(parentPath: Seq[Int], startIndex: Int, endIndex: Int) extends StepUpdateProps
case class StepReplacementProps(path: Seq[Int], newSteps: Seq[Step]) extends StepUpdateProps {
  def updateStepsFrom(multipleStepReplacementProps: MultipleStepReplacementProps, offset: Int): StepReplacementProps = {
    copy(newSteps = multipleStepReplacementProps.newSteps.slice(path.last + offset - multipleStepReplacementProps.startIndex, path.last + offset - multipleStepReplacementProps.startIndex + newSteps.length))
  }
  def getNewSteps: Seq[DisplayStep] = GetDisplaySteps(newSteps, path.init, path.last)
}
case class MultipleStepReplacementProps(parentPath: Seq[Int], startIndex: Int, endIndex: Int, newSteps: Seq[Step]) extends StepUpdateProps {
  def updateStepsFrom(multipleStepReplacementProps: MultipleStepReplacementProps): MultipleStepReplacementProps = {
    copy(newSteps = multipleStepReplacementProps.newSteps.slice(startIndex - multipleStepReplacementProps.startIndex, startIndex - multipleStepReplacementProps.startIndex + newSteps.length))
  }
  def getNewSteps: Seq[DisplayStep] = GetDisplaySteps(newSteps, parentPath, startIndex)
}
case class InsertionAndReplacementProps(insertion: StepInsertionProps, replacement: StepReplacementProps) extends StepUpdateProps
case class InsertionAndMultipleReplacementProps(insertion: StepInsertionProps, replacement: MultipleStepReplacementProps) extends StepUpdateProps
case class InsertionAndDeletionProps(insertion: StepInsertionProps, deletion: StepDeletionProps) extends StepUpdateProps
