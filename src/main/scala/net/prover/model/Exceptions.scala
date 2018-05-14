package net.prover.model

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
