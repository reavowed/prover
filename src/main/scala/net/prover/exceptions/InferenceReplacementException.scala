package net.prover.exceptions

import net.prover.model.proof.StepContext

trait InferenceReplacementException extends Exception

object InferenceReplacementException {
  case class AtStep(message: String, stepPath: Seq[Int]) extends Exception(s"${stepPath.mkString(".")} - $message")
  object AtStep {
    def apply(message: String)(implicit stepContext: StepContext): AtStep = AtStep(message, stepContext.stepReference.stepPath)
  }
  case class AtTheorem(message: String, stepPath: Seq[Int], proofIndex: Int, theoremName: String) extends Exception(s"Theorem '$theoremName', proof $proofIndex ${stepPath.mkString(".")} - $message")
  case class AtBook(message: String, stepPath: Seq[Int], proofIndex: Int, theoremName: String, chapterName: String, bookName: String) extends Exception(s"Book $bookName, chapter $chapterName, theorem '$theoremName', proof $proofIndex ${stepPath.mkString(".")} - $message")
}
