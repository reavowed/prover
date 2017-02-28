package net.prover.model

case class ReferenceResolveException(
    reference: String,
    message: String)
  extends Exception(s"Could not resolve reference $reference: $message")

case class ParseException(
    message: String,
    line: BookLine)
  extends Exception(s"Parse error in book '${line.bookTitle}' (${line.fileName} line ${line.number}): $message\n${line.text}")

case class ArbitraryVariableException(message: String) extends Exception(message)
