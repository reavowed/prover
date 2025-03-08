package net.prover.parsing

case class ParseException(
    baseMessage: String,
    token: Option[Token] = None,
    cause: Option[Throwable] = None)
  extends Exception(
    ParseException.getMessage(baseMessage, token),
    cause.orNull)

object ParseException {
    def getMessage(baseMessage: String, tokenOption: Option[Token]): String = {
        tokenOption match {
            case Some(token) =>
                s"Error in ${token.contextDescription}, line ${token.lineNumber} col ${token.columnNumber}: $baseMessage"
            case None =>
                baseMessage
        }
    }
}
