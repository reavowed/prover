package net.prover.exceptions

import net.prover.entries.{ProofWithContext, StepWithContext}
import net.prover.exceptions.InferenceReplacementException.getMessage
import net.prover.model.proof.StepContext

case class InferenceReplacementException(message: String, stepContext: StepContext, proofWithContext: ProofWithContext)
  extends Exception(getMessage(message, stepContext, proofWithContext))

object InferenceReplacementException {
  def apply(message: String, stepWithContext: StepWithContext): InferenceReplacementException = {
    apply(message, stepWithContext.stepContext, stepWithContext.proofWithContext)
  }
  def getMessage(message: String, stepContext: StepContext, proofWithContext: ProofWithContext): String = {
    import proofWithContext._
    import theoremWithContext._
    import chapterWithContext._
    import bookWithContext._
    s"Book ${book.title}, chapter ${chapter.title}, theorem '${theoremWithContext.theorem.title}', proof $proofIndex ${stepContext.stepReference.serialize} - $message"
  }
}
