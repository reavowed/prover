package net.prover.parsing

case class ParseException(
    message: String,
    cause: Option[Throwable] = None)
  extends Exception(
    message,
    cause.orNull)

object ParseException {
    trait NoWrap
}
