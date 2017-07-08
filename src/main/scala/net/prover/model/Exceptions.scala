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

case class ArbitraryVariableException(message: String) extends Exception(message)

case class DistinctVariableViolationException(variable: TermVariable)
  extends Exception(s"Distinct variable violated: $variable")
