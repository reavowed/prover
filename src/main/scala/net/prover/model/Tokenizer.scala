package net.prover.model

import java.nio.file.{Files, Path}

import scala.collection.mutable

case class Token(text: String, contextDescription: String, lineNumber: Int, columnNumber: Int)

case class TokenStream(tokens: Vector[Token], endToken: Token, lines: Vector[Vector[Char]], index: Int) {
  def isEmpty: Boolean = index == tokens.length
  def currentToken: Token = {
    if (isEmpty) throw new ParseException("No tokens remaining")
    tokens(index)
  }
  def advance(): TokenStream = copy(index = index + 1)
  def restOfLine(): (String, TokenStream) = {
    def findNextLine(lineNumber: Int, currentIndex: Int): Int = {
      if (currentIndex == tokens.length || tokens(currentIndex).lineNumber != lineNumber)
        currentIndex
      else
        findNextLine(lineNumber, currentIndex + 1)
    }
    val token = currentToken
    val text = lineSubstring(token.lineNumber - 1, token.columnNumber - 1)
    val nextIndex = findNextLine(currentToken.lineNumber, index)
    (text, copy(index = nextIndex))
  }
  def untilCloseParen(): (String, TokenStream) = {
    val initialLineNumber = currentToken.lineNumber
    def findCloseParen(currentIndex: Int, parenDepth: Int): Option[Int] = {
      if (parenDepth == 0)
        Some(currentIndex - 1)
      else if (currentIndex == tokens.length)
        None
      else if (tokens(currentIndex).lineNumber != initialLineNumber)
        None
      else if (tokens(currentIndex).text == "(")
        findCloseParen(currentIndex + 1, parenDepth + 1)
      else if (tokens(currentIndex).text == ")")
        findCloseParen(currentIndex + 1, parenDepth - 1)
      else
        findCloseParen(currentIndex + 1, parenDepth)
    }
    val closeParenIndex = findCloseParen(index, 1).getOrElse(throwParseException("No matching close-paren", None))
    (lineSubstring(initialLineNumber - 1, currentToken.columnNumber - 1, tokens(closeParenIndex).columnNumber - 1), copy(index = closeParenIndex))
  }

  private def lineSubstring(lineIndex: Int, start: Int): String = {
    lineSubstring(lineIndex, start, lines(lineIndex).length)
  }
  private def lineSubstring(lineIndex: Int, start: Int, end: Int): String = {
    lines(lineIndex).subSequence(start, end).toString
  }

  def throwParseException(message: String, cause: Option[Throwable]): Nothing = {
    val token = if (isEmpty) endToken else tokens(index)
    throw ParseException(
      s"Error in ${token.contextDescription}, line ${token.lineNumber} col ${token.columnNumber}: $message",
      cause)
  }
}

trait Tokenizer {
  def contextDescription: String
  def currentLine: Int
  def currentColumn: Int

  def isEmpty: Boolean
  def readNext(): (String, Tokenizer)
  def readUntilEndOfLine(): (String, Tokenizer)
  def readUntilCloseParen(): (String, Tokenizer)

  def throwParseException(message: String, cause: Option[Throwable]): Nothing = {
    throw ParseException(
      s"Error in $contextDescription, line $currentLine col $currentColumn: $message",
      cause)
  }
}

object Tokenizer {
  private val singleCharacterTokens = "(){},"
  def fromString(str: String, context: String): TokenStream = {
    val lines = str.lines.toVector.map(_.toVector)
    def findEndOfWhitespace(currentLine: Int, currentColumn: Int): Option[(Int, Int)] = {
      if (currentLine == lines.length)
        None
      else if (currentColumn == lines(currentLine).length)
        findEndOfWhitespace(currentLine + 1, 0)
      else if (lines(currentLine)(currentColumn).isWhitespace)
        findEndOfWhitespace(currentLine, currentColumn + 1)
      else
        Some((currentLine, currentColumn))
    }
    def findEndOfToken(currentLine: Int, currentColumn: Int): Int = {
      if (currentColumn == lines(currentLine).length || lines(currentLine)(currentColumn).isWhitespace || singleCharacterTokens.contains(lines(currentLine)(currentColumn)))
        currentColumn
      else
        findEndOfToken(currentLine, currentColumn + 1)
    }
    def getTokens(builder: mutable.Builder[Token, Vector[Token]], currentLine: Int, currentColumn: Int): Vector[Token] = {
      findEndOfWhitespace(currentLine, currentColumn) match {
        case Some((endOfWhitespaceLine, endOfWhitespaceColumn)) =>
          if (singleCharacterTokens.contains(lines(endOfWhitespaceLine)(endOfWhitespaceColumn)))
            getTokens(
              builder += Token(
                lines(endOfWhitespaceLine)(endOfWhitespaceColumn).toString,
                context,
                endOfWhitespaceLine + 1,
                endOfWhitespaceColumn + 1),
              endOfWhitespaceLine,
              endOfWhitespaceColumn + 1)
          else {
            val endOfTokenColumn = findEndOfToken(endOfWhitespaceLine, endOfWhitespaceColumn + 1)
            getTokens(
              builder += Token(
                lines(endOfWhitespaceLine).subSequence(endOfWhitespaceColumn, endOfTokenColumn).toString,
                context,
                endOfWhitespaceLine + 1,
                endOfWhitespaceColumn + 1),
              endOfWhitespaceLine,
              endOfTokenColumn)
          }
        case None =>
          builder.result()
      }
    }
    TokenStream(getTokens(Vector.newBuilder, 0, 0), Token("", context, lines.length, lines.last.length), lines, 0)
  }
  def fromPath(path: Path, context: String): TokenStream = {
    fromString(new String(Files.readAllBytes(path), "UTF-8"), s"$context ($path)")
  }
}
