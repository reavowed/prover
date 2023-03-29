package net.prover.exceptions

import net.prover.entries.StepWithContext
import net.prover.exceptions.InferenceReplacementException.getMessage

case class InferenceReplacementException(message: String, stepWithContext: StepWithContext)
  extends Exception(getMessage(message, stepWithContext))

object InferenceReplacementException {
  def getMessage(message: String, stepWithContext: StepWithContext): String = {
    import stepWithContext._
    import parentContext._
    import proofWithContext._
    import theoremWithContext._
    s"Book ${book.title}, chapter ${chapter.title}, theorem '${theoremWithContext.theorem.title}', proof $proofIndex ${stepProvingContext.stepContext.stepReference.serialize} - $message"
  }
}
