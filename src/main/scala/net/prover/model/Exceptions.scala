package net.prover.model

import net.prover.model.components.TermVariable

case class ReferenceResolveException(
    reference: String,
    message: String)
  extends Exception(s"Could not resolve reference $reference: $message")

case class ParseException(
    message: String,
    cause: Option[Throwable] = None)
  extends Exception(
    message,
    cause.orNull)

object ParseException {
    trait NoWrap
}

case class ProvingException(
    message: String,
    fileName: String,
    lineNumber: Int)
  extends Exception(
    s"Proof error at $fileName line $lineNumber: $message")

case class ArbitraryVariableException(message: String)
  extends Exception(message)
  with ParseException.NoWrap

case class DistinctVariableViolationException(variable: TermVariable)
  extends Exception(s"Distinct variable violated: $variable")
  with ParseException.NoWrap
