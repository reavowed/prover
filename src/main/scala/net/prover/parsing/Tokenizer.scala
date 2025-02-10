package net.prover.parsing

import java.nio.file.{Files, Path}

case class Token(text: String, contextDescription: String, lineNumber: Int, columnNumber: Int)

case class TokenStream(tokens: Vector[Token], endToken: Token, lines: Vector[Vector[Char]], index: Int) {
  def isEmpty: Boolean = index == tokens.length
  def currentToken: Token = {
    if (isEmpty) throwParseException("No tokens remaining")
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
    val closeParenIndex = findCloseParen(index, 1).getOrElse(throwParseException("No matching close-paren"))
    (lineSubstring(initialLineNumber - 1, currentToken.columnNumber - 1, tokens(closeParenIndex).columnNumber - 1), copy(index = closeParenIndex))
  }

  private def lineSubstring(lineIndex: Int, start: Int): String = {
    lineSubstring(lineIndex, start, lines(lineIndex).length)
  }
  private def lineSubstring(lineIndex: Int, start: Int, end: Int): String = {
    lines(lineIndex).slice(start, end).mkString
  }

  def throwParseException(message: String, cause: Option[Throwable] = None): Nothing = {
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
  private val singleCharacterTokens = "(){},".toSet
  def fromString(str: String, context: String): TokenStream = {
    val lines = str.split("\\r?\\n").toVector.map(_.toVector)
    val numberOfLines = lines.length
    var lineIndex = 0
    val builder = Vector.newBuilder[Token]
    while (lineIndex < numberOfLines) {
      val line = lines(lineIndex)
      val lineLength = line.length
      var columnIndex = 0
      while (columnIndex < lineLength) {
        val char = line(columnIndex)
        if (char.isWhitespace) {
          columnIndex += 1
        } else if (singleCharacterTokens.contains(char)) {
          builder += Token(char.toString, context, lineIndex + 1, columnIndex + 1)
          columnIndex += 1
        } else {
          var endColumnIndex = columnIndex + 1
          while (endColumnIndex < lineLength && !line(endColumnIndex).isWhitespace && !singleCharacterTokens.contains(line(endColumnIndex))) {
            endColumnIndex += 1
          }
          builder += Token(line.slice(columnIndex, endColumnIndex).mkString, context, lineIndex + 1, columnIndex + 1)
          columnIndex = endColumnIndex
        }
      }
      lineIndex += 1
    }
    TokenStream(builder.result(), Token("", context, lines.length, lines.last.length), lines, 0)
  }

  def fromPath(path: Path, context: String): TokenStream = {
    fromString(new String(Files.readAllBytes(path), "UTF-8"), s"$context ($path)")
  }
}
