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
case class StepInsertionProps(path: Seq[Int], newSteps: Seq[Step]) extends StepUpdateProps
case class StepDeletionProps(parentPath: Seq[Int], startIndex: Int, endIndex: Int) extends StepUpdateProps
case class StepReplacementProps(path: Seq[Int], newSteps: Seq[Step]) extends StepUpdateProps
case class MultipleStepReplacementProps(parentPath: Seq[Int], startIndex: Int, endIndex: Int, newSteps: Seq[Step]) extends StepUpdateProps
case class InsertionAndReplacementProps(insertion: StepInsertionProps, replacement: StepReplacementProps) extends StepUpdateProps
case class InsertionAndMultipleReplacementProps(insertion: StepInsertionProps, replacement: MultipleStepReplacementProps) extends StepUpdateProps
case class InsertionAndDeletionProps(insertion: StepInsertionProps, deletion: StepDeletionProps) extends StepUpdateProps
