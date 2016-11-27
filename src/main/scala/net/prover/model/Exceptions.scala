package net.prover.model

case class ReferenceResolveException(
    reference: String,
    message: String)
  extends Exception(s"Could not resolve reference $reference: $message")

case class ParseException(
    line: BookLine,
    message: String)
  extends Exception(s"Could not parse line ${line.number}: $message\n${line.text}")

object ParseException {
  def withMessage(message: String, line: BookLine) = ParseException(line, message)
  def fromCause(cause: Throwable, line: BookLine) = ParseException(line, cause.getMessage)
}
